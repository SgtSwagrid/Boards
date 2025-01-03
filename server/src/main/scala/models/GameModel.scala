package models

import boards.Catalogue
import boards.dsl.meta.Game
import boards.dsl.meta.PlayerRef.PlayerId
import boards.dsl.states.GameState
import boards.protocol.GameProtocol.*
import boards.protocol.Room
import boards.protocol.Room.{Player, Status}
import boards.protocol.UserProtocol.User
import org.mindrot.jbcrypt.BCrypt
import slick.dbio.{DBIO, DBIOAction}
import slick.jdbc.H2Profile.api.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import io.circe.*
import io.circe.parser.*
import schema.{InputTable, PlayerTable, RoomTable}
import schema.RoomTable.given
import schema.InputTable.InputRow
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
    yield Catalogue.byName(room.get.gameId)
    
  def joinRoom(roomId: String, userId: Int): Future[PlayerRow] =
    val action = for
      room <- Action.getRoom(roomId)
      if room.isPending
      players <- Action.getAllPlayers(roomId)
      if players.size < room.maxPlayers
      player = PlayerRow(userId, room.id, players.size, false)
      _ <- PlayerTable.players += player
    yield player
    db.run(action)
    
  def leaveRoom(roomId: String)(positions: PlayerId*): Future[Unit] =
    
    def f(A: Seq[Int], x: Int): Seq[Int] = A match
      case head :: tail => (if head > x then head - 1 else head) +: f(tail, x)
      case Nil => Nil
      
    def g(A: Seq[Int]): Seq[Int] = A match
      case head :: tail => head +: g(f(tail, head))
      case Nil => Nil
    
    val action = DBIO.seq (
      g(positions.map(_.toInt)).map(PlayerId(_)).map: position =>
        for
          room <- Action.getRoom(roomId)
          if room.isPending
          player <- Action.getPlayerByPos(roomId, position)
          _ <- Query.playersByPos(roomId)(position).delete
          updates <- Query.playersAfter(roomId, position).result
            .map(_.map(p => p.copy(position = p.position - 1)))
          _ <- Query.playersAfter(roomId, position).delete
          _ <- PlayerTable.players ++= updates
        yield ()
    *)
    db.run(action).map(_ => ())
    
  def swapPlayers(roomId: String)(leftId: PlayerId, rightId: PlayerId): Future[Unit] =
    val action = for
      room <- Action.getRoom(roomId)
      if room.isPending
      Seq(left, right) <- Action.getPlayersByPos(roomId)(leftId, rightId)
      _ <- Query.playersByPos(roomId)(leftId, rightId).delete
      _ <- PlayerTable.players ++= Seq (
        left.copy(position = rightId.toInt),
        right.copy(position = leftId.toInt),
      )
    yield ()
    db.run(action.transactionally)
    
  def getPlayers(roomId: String): Future[Seq[Player]] =
    val action = for
      players <- Action.getAllPlayers(roomId)
      users <- DBIO.sequence(players.map(_.userId).map(UserModel().Action.getUserById))
    yield (players.zip(users).map: (p, u) =>
      Player(u.id, u.username, p.roomId, PlayerId(p.position), p.isOwner, p.hasResigned, p.hasOfferedDraw)
    ).sortBy(_.position)
    db.run(action)
    
  def setProperty(roomId: String, property: String, value: Int): Future[Room] =
    val action = for
      room <- Action.getRoom(roomId)
      if room.isPending
      updated = room.copy(properties = room.properties + (property -> value))
      _ <- RoomTable.rooms.insertOrUpdate(updated)
    yield updated
    db.run(action).recover{e => e.printStackTrace(); ???}
    
  def startGame(roomId: String): Future[Room] =
    val action = for
      room <- Action.getRoom(roomId)
      if room.isPending
      numPlayers <- Action.getNumPlayers(roomId)
      if room.requiredNumPlayers.contains(numPlayers)
      _ <- Query.room(roomId).map(_.status).update(Status.Active)
    yield room.copy(status = Status.Active)
    db.run(action.transactionally).recover{e => e.printStackTrace(); ???}
    
  def takeAction(roomId: String, userId: Int, inputId: Int, end: Boolean = false): Future[InputRow] =
    val action = for
      room <- Action.getRoom(roomId)
      if room.status.isActive
      actions <- Action.getInputHistory(roomId)
      time = System.currentTimeMillis()
      action = InputRow(roomId, actions.size, userId, inputId, time)
      _ <- InputTable.actions += action
      _ <- if end
        then Query.room(roomId).map(_.status).update(Status.Complete)
        else DBIO.successful(())
    yield action
    db.run(action.transactionally).recover{e => e.printStackTrace(); ???}
    
  def resign(roomId: String, resigned: Boolean = true)(positions: PlayerId*): Future[Unit] =
    val action = for
      room <- Action.getRoom(roomId)
      if room.isActive
      players <- Action.getPlayersByPos(roomId)(positions*)
      _ <- Query.playersByPos(roomId)(positions*).delete
      _ <- PlayerTable.players ++= players.map(_.copy (
        hasResigned = resigned,
        hasOfferedDraw = false,
      ))
      nonResigned <- Action.getAllPlayers(roomId).map(_.filter(!_.hasResigned))
      _ <- if nonResigned.sizeIs == 1
        then Query.room(roomId).map(_.status).update(Status.Complete)
        else DBIO.successful(())
    yield ()
    db.run(action.transactionally)
    
  def offerDraw(roomId: String, draw: Boolean = true)(positions: PlayerId*): Future[Unit] =
    val action = for
      room <- Action.getRoom(roomId)
      if room.isActive
      players <- Action.getPlayersByPos(roomId)(positions*).map(_.filter(!_.hasResigned))
      _ <- Query.playersByPos(roomId)(positions*).delete
      _ <- PlayerTable.players ++= players.map(_.copy (
        hasResigned = false,
        hasOfferedDraw = draw,
      ))
      active <- Action.getAllPlayers(roomId).map(_.filter(p => !p.hasOfferedDraw && !p.hasResigned))
      _ <- if active.isEmpty
        then Query.room(roomId).map(_.status).update(Status.Complete)
        else DBIO.successful(())
    yield ()
    db.run(action.transactionally)
    
  def getGameState(roomId: String): Future[GameState] =
    val action = for
      room <- Action.getRoom(roomId)
      numPlayers <- Action.getNumPlayers(roomId)
      inputs <- Action.getInputHistory(roomId)
      requiredPlayers = room.game.numPlayers.filter(_ >= numPlayers).min
      initial = room.game.initial(Game.GameConfig(requiredPlayers, room.properties))
      current = inputs.foldLeft[GameState](initial): (state, action) =>
        state.applyInputById(action.inputId).get
    yield current
    db.run(action).recover{e => e.printStackTrace(); ???}
    
  private[models] object Action:
    
    def getAllPlayers(roomId: String): DBIO[Seq[PlayerRow]] =
      Query.allPlayers(roomId).result
      
    def getNumPlayers(roomId: String): DBIO[Int] =
      getAllPlayers(roomId).map(_.size)
    
    def getPlayersByUser(roomId: String, userId: Int): DBIO[Seq[PlayerRow]] =
      Query.playersByUser(roomId, userId).result
      
    def getPlayerByPos(roomId: String, position: PlayerId): DBIO[PlayerRow] =
      getPlayerByPosOption(roomId, position).map(_.get)
      
    def getPlayerByPosOption(roomId: String, position: PlayerId): DBIO[Option[PlayerRow]] =
      Query.playersByPos(roomId)(position).result.headOption
      
    def getPlayersByPos(roomId: String)(positions: PlayerId*): DBIO[Seq[PlayerRow]] =
      Query.playersByPos(roomId)(positions*).result
      
    def getRoom(roomId: String): DBIO[Room] =
      getRoomOption(roomId).map(_.get)
      
    def getRoomOption(roomId: String): DBIO[Option[Room]] =
     Query.room(roomId).result.headOption
      
    def getInputHistory(roomId: String): DBIO[Seq[InputRow]] =
      Query.actions(roomId).result
      
  private[models] object Query:
    
    def allPlayers(roomId: String): Query[PlayerTable, PlayerRow, Seq] =
      PlayerTable.players
        .filter(_.roomId === roomId)
        .sortBy(_.position)
    
    def playersByUser(roomId: String, userId: Int): Query[PlayerTable, PlayerRow, Seq] =
      allPlayers(roomId).filter(_.userId === userId)
      
    def players(roomId: String)(userIds: Int*): Query[PlayerTable, PlayerRow, Seq] =
      allPlayers(roomId).filter(_.userId.inSet(userIds))
      
    def playersByPos(roomId: String)(positions: PlayerId*): Query[PlayerTable, PlayerRow, Seq] =
      allPlayers(roomId).filter(_.position.inSet(positions.map(_.toInt)))
      
    def playersBefore(roomId: String, position: PlayerId): Query[PlayerTable, PlayerRow, Seq] =
      allPlayers(roomId).filter(_.position < position.toInt)
      
    def playersAfter(roomId: String, position: PlayerId): Query[PlayerTable, PlayerRow, Seq] =
      allPlayers(roomId).filter(_.position > position.toInt)
      
    def actions(roomId: String): Query[InputTable, InputRow, Seq] =
      InputTable.actions.filter(_.roomId === roomId).sortBy(_.position)
      
    def room(roomId: String): Query[RoomTable, Room, Seq] =
      RoomTable.rooms.filter(_.id === roomId)
    
  private def generateRoomId(length: Int = 5): String =
    Random.between(0, 1 << (4 * length)).toHexString.toUpperCase.padTo(length, '0')