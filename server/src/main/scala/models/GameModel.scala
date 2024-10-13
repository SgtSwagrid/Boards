package models

import boards.Games
import boards.algebra.{Action, Game, GameState}
import boards.protocol.GameProtocol.{Player, Spectator, Room, Participant, Status, Unregistered}
import org.mindrot.jbcrypt.BCrypt
import slick.dbio.{DBIO, DBIOAction}
import slick.jdbc.H2Profile.api.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import io.circe.*
import io.circe.parser.*
import schema.{ActionTable, PlayerTable, RoomTable}
import schema.RoomTable.given
import schema.ActionTable.ActionRow
import schema.PlayerTable.PlayerRow

import scala.util.Random
import scala.concurrent.{ExecutionContext, Future}

class GameModel(using db: Database, ec: ExecutionContext):
  
  def createRoom(userId: Int, gameId: String): Future[Room] =
    val room = Room(generateRoomId(), gameId, Status.Pending)
    val player = PlayerRow(userId, room.id, 0, true)
    val action = for
      _ <- RoomTable.rooms += room
      _ <- PlayerTable.players += player
    yield room
    db.run(action)
    
  def getRoomById(roomId: String): Future[Option[Room]] =
    db.run(Action.getRoomOption(roomId))
    
  def getGame(roomId: String): Future[Game] =
    for room <- getRoomById(roomId)
    yield Games.byName(room.get.gameId)
    
  def joinRoom(roomId: String, userId: Int): Future[PlayerRow] =
    val action = for
      room <- Action.getRoom(roomId)
      if room.status.isPending
      players <- Action.getAllPlayers(roomId)
      if players.size < room.game.numPlayers.max
      if !players.exists(_.userId == userId)
      player = PlayerRow(userId, room.id, players.size, false)
      _ <- PlayerTable.players += player
    yield player
    db.run(action)
    
  def leaveRoom(roomId: String, userId: Int): Future[Boolean] =
    val action = for
      room <- Action.getRoom(roomId)
      if room.status.isPending
      player <- Action.getPlayer(roomId, userId)
      _ <- Query.player(roomId, userId).delete
      updates <- Query.playersAfter(roomId, player.position).result
        .map(_.map(p => p.copy(position = p.position - 1)))
      _ <- Query.playersAfter(roomId, player.position).delete
      _ <- PlayerTable.players ++= updates
    yield true
    db.run(action.transactionally.asTry.map(_.getOrElse(false)))
    
  def reorderPlayer(roomId: String, userId: Int): Future[Unit] =
    val action = for
      room <- Action.getRoom(roomId)
      if room.status.isPending
      player <- Action.getPlayer(roomId, userId)
      if player.position > 0
      previous <- Action.getPlayerByPos(roomId, player.position - 1)
      _ <- Query.players(roomId, userId, previous.userId).delete
      _ <- PlayerTable.players ++= Seq (
        player.copy(position = previous.position),
        previous.copy(position = player.position),
      )
    yield true
    db.run(action.transactionally.asTry.map(_.getOrElse(false)))
    
  def getPlayers(roomId: String): Future[Seq[Player]] =
    val action = for
      players <- Action.getAllPlayers(roomId)
      users <- DBIO.sequence(players.map(_.userId).map(UserModel().Action.getUserById))
    yield (players.zip(users).map: (p, u) =>
      Player(u.id, p.roomId, p.position, p.isOwner, u.username)
    ).sortBy(_.position)
    db.run(action)
    
  def getParticipant(roomId: String, userId: Option[Int]): Future[Participant] =
    userId match
      case None => Future.successful(Unregistered)
      case Some(userId) =>
        val action = for
          user <- UserModel().Action.getUserOptionById(userId)
          player <- Action.getPlayerOption(roomId, userId)
        yield (user, player) match
          case (None, _) => Unregistered
          case (Some(user), None) => Spectator(user.id, user.username)
          case (Some(user), Some(player)) =>
            Player(user.id, roomId, player.position, player.isOwner, user.username)
        db.run(action)
    
  def startGame(roomId: String): Future[Room] =
    val action = for
      room <- Action.getRoom(roomId)
      if room.status.isPending
      numPlayers <- Action.getNumPlayers(roomId)
      if room.game.numPlayers.contains(numPlayers)
      _ <- Query.room(roomId).map(_.status).update(Status.Active)
    yield room.copy(status = Status.Active)
    db.run(action.transactionally)
    
  def takeAction(roomId: String, userId: Int, actionHash: String, end: Boolean = false): Future[ActionRow] =
    val action = for
      room <- Action.getRoom(roomId)
      if room.status.isActive
      actions <- Action.getActions(roomId)
      time = System.currentTimeMillis()
      action = ActionRow(roomId, actions.size, userId, actionHash, time)
      _ <- ActionTable.actions += action
      _ <- if end
        then Query.room(roomId).map(_.status).update(Status.Complete)
        else DBIO.successful(())
    yield action
    db.run(action.transactionally)
    
  def getGameState(roomId: String): Future[GameState] =
    val action = for
      room <- Action.getRoom(roomId)
      numPlayers <- Action.getNumPlayers(roomId)
      actions <- Action.getActions(roomId)
      requiredPlayers = room.game.numPlayers.filter(_ >= numPlayers).min
      initial = room.game.initial(Game.GameConfig(requiredPlayers))
      current = actions.foldLeft[GameState](initial): (state, action) =>
        state.takeActionByHash(action.actionHash).get
    yield current
    db.run(action)
    
  private[models] object Action:
    
    def getAllPlayers(roomId: String): DBIO[Seq[PlayerRow]] =
      Query.allPlayers(roomId).result
      
    def getNumPlayers(roomId: String): DBIO[Int] =
      getAllPlayers(roomId).map(_.size)
    
    def getPlayer(roomId: String, userId: Int): DBIO[PlayerRow] =
      getPlayerOption(roomId, userId).map(_.get)
    
    def getPlayerOption(roomId: String, userId: Int): DBIO[Option[PlayerRow]] =
      Query.player(roomId, userId).result.headOption
      
    def getPlayerByPos(roomId: String, position: Int): DBIO[PlayerRow] =
      getPlayerByPosOption(roomId, position).map(_.get)
      
    def getPlayerByPosOption(roomId: String, position: Int): DBIO[Option[PlayerRow]] =
      Query.playerByPos(roomId, position).result.headOption
      
    def getRoom(roomId: String): DBIO[Room] =
      getRoomOption(roomId).map(_.get)
      
    def getRoomOption(roomId: String): DBIO[Option[Room]] =
     Query.room(roomId).result.headOption
      
    def getActions(roomId: String): DBIO[Seq[ActionRow]] =
      Query.actions(roomId).result
      
  private[models] object Query:
    
    def allPlayers(roomId: String): Query[PlayerTable, PlayerRow, Seq] =
      PlayerTable.players
        .filter(_.roomId === roomId)
        .sortBy(_.position)
    
    def player(roomId: String, userId: Int): Query[PlayerTable, PlayerRow, Seq] =
      allPlayers(roomId).filter(_.userId === userId)
      
    def players(roomId: String, userIds: Int*): Query[PlayerTable, PlayerRow, Seq] =
      allPlayers(roomId).filter(_.userId.inSet(userIds))
      
    def playerByPos(roomId: String, position: Int): Query[PlayerTable, PlayerRow, Seq] =
      allPlayers(roomId).filter(_.position === position)
      
    def playersBefore(roomId: String, position: Int): Query[PlayerTable, PlayerRow, Seq] =
      allPlayers(roomId).filter(_.position < position)
      
    def playersAfter(roomId: String, position: Int): Query[PlayerTable, PlayerRow, Seq] =
      allPlayers(roomId).filter(_.position > position)
      
    def actions(roomId: String): Query[ActionTable, ActionRow, Seq] =
      ActionTable.actions.filter(_.roomId === roomId).sortBy(_.position)
      
    def room(roomId: String): Query[RoomTable, Room, Seq] =
      RoomTable.rooms.filter(_.id === roomId)
    
  private def generateRoomId(): String =
    Random.between(0, 1 << (4 * 5)).toHexString.toUpperCase.padTo(5, '0')