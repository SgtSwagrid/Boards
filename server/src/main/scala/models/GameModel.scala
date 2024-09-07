package models

import boards.Games
import boards.algebra.{Action, Game, GameState}
import boards.protocol.GameProtocol.Player
import org.mindrot.jbcrypt.BCrypt
import slick.dbio.{DBIO, DBIOAction}
import slick.jdbc.H2Profile.api.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import io.circe.*
import io.circe.parser.*
import schema.{ActionTable, PlayerTable, RoomTable}
import schema.ActionTable.ActionRow
import schema.RoomTable.{RoomRow, Status}
import schema.PlayerTable.PlayerRow

import scala.util.Random
import scala.concurrent.{ExecutionContext, Future}

class GameModel(using db: Database, ec: ExecutionContext):
  
  def createRoom(userId: Int, gameId: Int): Future[RoomRow] =
    val room = RoomRow(generateRoomId(), gameId, Status.Pending)
    val player = PlayerRow(userId, room.id, 0, true)
    val action = for
      _ <- RoomTable.rooms += room
      _ <- PlayerTable.players += player
    yield room
    db.run(action)
    
  def getRoomById(id: String): Future[Option[RoomRow]] =
    db.run(RoomTable.rooms.filter(_.id === id).result.map(_.headOption))
    
  def getGame(roomId: String): Future[Game] =
    for room <- getRoomById(roomId)
    yield Games.all(room.get.gameId)
    
  def joinRoom(userId: Int, roomId: String): Future[PlayerRow] =
    for
      room <- getRoomById(roomId)
      players <- getPlayers(roomId)
      if room.exists(r => Games.all(r.gameId).numPlayers.max > players.size)
      player = PlayerRow(userId, room.get.id, players.size, false)
      _ <- db.run(PlayerTable.players += player)
    yield player
    
  def getPlayers(roomId: String): Future[Seq[Player]] =
    for
      players <- db.run(PlayerTable.players.filter(_.roomId === roomId).result)
      users <- Future.sequence(players.map(_.userId).map(UserModel().getUserById))
    yield (players.zip(users.flatten).map: (p, u) =>
      Player(u.id, p.roomId, p.position, p.isOwner, u.username)
    ).sortBy(_.position)
    
  def getActions(roomId: String): Future[Seq[ActionRow]] =
    db.run(ActionTable.actions.filter(_.roomId === roomId).sortBy(_.position).result)
    
  def takeAction(roomId: String, userId: Int, actionHash: Int): Future[ActionRow] =
    for
      room <- getRoomById(roomId).map(_.get)
      actions <- getActions(roomId)
      time = System.currentTimeMillis()
      action = ActionRow(roomId, actions.size, userId, actionHash, time)
      _ <- db.run(ActionTable.actions += action)
    yield action
    
  def getGameState(roomId: String): Future[GameState] =
    for
      room <- getRoomById(roomId).map(_.get)
      players <- getPlayers(roomId)
      actions <- getActions(roomId)
      game = Games.all(room.gameId)
      requiredPlayers = game.numPlayers.filter(_ >= players.size).min
      initial = game.initial(requiredPlayers)
      current = actions.foldLeft[GameState](initial): (state, action) =>
        state.takeActionByHash(action.actionHash).get
    yield current
    
  private def generateRoomId(): String =
    Random.between(0, 1 << (4 * 5)).toHexString.toUpperCase.padTo(5, '0')