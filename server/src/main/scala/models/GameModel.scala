package models

import boards.GameCatalogue
import boards.dsl.meta.Game
import boards.dsl.meta.PlayerRef.PlayerId
import boards.dsl.meta.TurnId
import boards.dsl.meta.TurnId.{*, given}
import boards.dsl.states.GameState
import boards.bots.BotCatalogue
import boards.protocol.GameProtocol.*
import boards.protocol.Room
import boards.protocol.Room.{Player, PlayerIdentity, RoomWithPlayers, RichRoom, Status}
import boards.protocol.UserProtocol.User
import org.mindrot.jbcrypt.BCrypt
import slick.dbio.{DBIO, DBIOAction}
import slick.jdbc.H2Profile.api.*
import schema.{InputTable, PlayerTable, RoomTable}
import schema.RoomTable.given
import schema.InputTable.InputRow
import schema.PlayerTable.PlayerRow

import scala.util.Random
import scala.concurrent.{ExecutionContext, Future}

class GameModel(using db: Database, ec: ExecutionContext):
  
  def createRoom (
    userId:     Int,
    gameId:     String,
    numPlayers: Int              = 1,
    properties: Map[String, Int] = Map.empty,
    forkedFrom: Option[String]   = None,
    forkedTurn: Option[TurnId]   = None,
    rematchOf:  Option[String]   = None,
  ): Future[Room] =
    
    val room     = Room (
      id         = generateRoomId(),
      gameId     = gameId,
      status     = Status.Pending,
      properties = properties,
      seed       = Random.nextLong(),
      forkedFrom = forkedFrom,
      forkedTurn = forkedTurn,
      rematchOf  = rematchOf,
      rematch    = None,
    )
    
    val players = (0 until numPlayers).map: i =>
      PlayerRow (
        userId   = Some(userId),
        roomId   = room.id,
        position = i,
        isOwner  = true,
      )
    
    db.run:
      for
        _ <- RoomTable.rooms += room
        _ <- PlayerTable.players ++= players
        
        // If the room is a fork, copy the input history from the original room.
        _ <- forkedFrom zip forkedTurn match
          case Some((forkedFrom, forkedTurn)) =>
            for
              inputs <- DBEffect.getInputHistory(forkedFrom)
              _ <- InputTable.inputs ++= inputs.take(forkedTurn.toInt)
                .map(_.copy(roomId = room.id))
            yield ()
          case None => DBIO.successful(())
        
        // If the room is a rematch, update the original room to point to the new one.
        _ <- rematchOf match
          case Some(rematchOf) => DBQuery.room(rematchOf).map(_.rematch).update(Some(room.id))
          case None => DBIO.successful(())
          
      yield room
      
  def forkRoom (
    userId: Int,
    roomId: String,
    turnId: TurnId,
  ): Future[Room] =
    
    for
      original <- getRoomById(roomId).map(_.get)
      fork <- createRoom (
        userId,
        original.gameId,
        numPlayers=1,
        properties=original.properties,
        forkedFrom=Some(roomId),
        forkedTurn=Some(turnId),
      )
    yield fork
    
  def createOrJoinRematch (
    userId: Int,
    roomId: String,
  ): Future[Room] =
    
    for
      original <- getRoomById(roomId).map(_.get)
      numPlayers <- getPlayers(roomId).map(_.count(_.userIdOpt.contains(userId)))
      rematch <- original.rematch match
        case Some(rematchId) =>
          for
            rematch <- getRoomById(rematchId).map(_.get)
            _ <- joinRoom(rematchId, userId, numPlayers)
          yield rematch
        case None => createRoom (
          userId,
          original.gameId,
          numPlayers=numPlayers,
          properties=original.properties,
          rematchOf=Some(roomId),
        )
    yield rematch
    
  def getRoomById (roomId: String): Future[Option[Room]] =
    db.run(DBEffect.getRoomOption(roomId))
    
  def getRoomWithPlayers (roomId: String): Future[RoomWithPlayers] =
    for
      room <- getRoomById(roomId).map(_.get)
      players <- getPlayers(roomId)
    yield RoomWithPlayers(room, players)
    
  def getRichRoom( roomId: String): Future[RichRoom] =
    
    for
      room <- getRoomWithPlayers(roomId)
      forkedFrom <- room.forkedFrom match
        case Some(forkedFrom) => getRoomWithPlayers(forkedFrom).map(Some.apply)
        case None => Future.successful(None)
      rematchOf <- room.rematchOf match
        case Some(rematchOf) => getRoomWithPlayers(rematchOf).map(Some.apply)
        case None => Future.successful(None)
      rematch <- room.rematch match
        case Some(rematch) => getRoomWithPlayers(rematch).map(Some.apply)
        case None => Future.successful(None)
    yield RichRoom(room, forkedFrom, rematchOf, rematch)
    
  def getGame (roomId: String): Future[Game] =
    
    for room <- getRoomById(roomId)
    yield GameCatalogue.byName(room.get.gameId)
    
  def joinRoom (roomId: String, userId: Int, multiplicity: Int = 1): Future[Seq[PlayerRow]] = db.run:
    
      for
        room <- DBEffect.getRoom(roomId)
        if room.isPending
        existingPlayers <- DBEffect.getAllPlayers(roomId)
        if existingPlayers.size < room.maxPlayers
        newPlayerIds = (existingPlayers.size until existingPlayers.size + multiplicity)
          .takeWhile(_ < room.maxPlayers)
        newPlayers = newPlayerIds.map(id => PlayerRow(Some(userId), None, room.id, id, false))
        _ <- PlayerTable.players ++= newPlayers
      yield newPlayers
    
  def addBot (roomId: String, botId: String): Future[Unit] = db.run:
    
    for
      room <- DBEffect.getRoom(roomId)
      if BotCatalogue.byId.contains(botId)
      if room.isPending
      existingPlayers <- DBEffect.getAllPlayers(roomId)
      if existingPlayers.size < room.maxPlayers
      position = existingPlayers.size
      bot = PlayerRow(None, Some(botId), roomId, position)
      _ <- PlayerTable.players += bot
    yield ()
    
  def leaveRoom (roomId: String) (positions: PlayerId*): Future[Unit] =
    
    def f (A: Seq[Int], x: Int): Seq[Int] = A match
      case head :: tail => (if head > x then head - 1 else head) +: f(tail, x)
      case Nil => Nil
      
    def g (A: Seq[Int]): Seq[Int] = A match
      case head :: tail => head +: g(f(tail, head))
      case Nil => Nil
    
    val action = DBIO.seq (
      g(positions.map(_.toInt)).map(PlayerId(_)).map: position =>
        for
          room <- DBEffect.getRoom(roomId)
          if room.isPending
          player <- DBEffect.getPlayerByPos(roomId, position)
          _ <- DBQuery.playersByPos(roomId)(position).delete
          updates <- DBQuery.playersAfter(roomId, position).result
            .map(_.map(p => p.copy(position = p.position - 1)))
          _ <- DBQuery.playersAfter(roomId, position).delete
          _ <- PlayerTable.players ++= updates
        yield ()
    *)
    db.run(action).map(_ => ())
    
  def swapPlayers (roomId: String) (leftId: PlayerId, rightId: PlayerId): Future[Unit] =
    val action = for
      room <- DBEffect.getRoom(roomId)
      if room.isPending
      Seq(left, right) <- DBEffect.getPlayersByPos(roomId)(leftId, rightId)
      _ <- DBQuery.playersByPos(roomId)(leftId, rightId).delete
      _ <- PlayerTable.players ++= Seq (
        left.copy(position = rightId.toInt),
        right.copy(position = leftId.toInt),
      )
    yield ()
    db.run(action.transactionally)
    
  def getPlayers (roomId: String): Future[Seq[Player]] =
    val action = for
      players <- DBEffect.getAllPlayers(roomId)
      users <- DBIO.sequence:
        players.map(_.userId).map: userIdOpt =>
          userIdOpt
            .map(UserModel().Action.getUserOptionById)
            .getOrElse(DBIO.successful(None))
    yield (players.zip(users).map: (p, userOpt) =>
      val playerType = userOpt match
        case Some(user) => PlayerIdentity.Human(user.id, user.username)
        case None => PlayerIdentity.Bot(p.botId.get)
      Player(playerType, p.roomId, PlayerId(p.position), p.isOwner, p.hasResigned, p.hasOfferedDraw)
    ).sortBy(_.position)
    db.run(action)
    
  def setProperty (roomId: String, property: String, value: Int): Future[Room] =
    val action = for
      room <- DBEffect.getRoom(roomId)
      if room.isPending
      updated = room.copy(properties = room.properties + (property -> value))
      _ <- RoomTable.rooms.insertOrUpdate(updated)
    yield updated
    db.run(action).recover{e => e.printStackTrace(); ???}
    
  def startGame (roomId: String): Future[Room] =
    val action = for
      room <- DBEffect.getRoom(roomId)
      if room.isPending
      numPlayers <- DBEffect.getNumPlayers(roomId)
      if room.possiblePlayerCounts.contains(numPlayers)
      _ <- DBQuery.room(roomId).map(_.status).update(Status.Active)
    yield room.copy(status = Status.Active)
    db.run(action.transactionally).recover{e => e.printStackTrace(); ???}
    
  def takeAction (roomId: String, inputId: Int, end: Boolean = false): Future[InputRow] =
    val action = for
      room <- DBEffect.getRoom(roomId)
      if room.status.isActive
      inputs <- DBEffect.getInputHistory(roomId)
      time = System.currentTimeMillis()
      input = InputRow(roomId, inputs.size, inputId, time)
      _ <- InputTable.inputs += input
      _ <- if end
        then DBQuery.room(roomId).map(_.status).update(Status.Complete)
        else DBIO.successful(())
    yield input
    db.run(action.transactionally).recover{e => e.printStackTrace(); ???}
    
  def resign (roomId: String, resigned: Boolean = true) (positions: PlayerId*): Future[Unit] =
    val action = for
      room <- DBEffect.getRoom(roomId)
      if room.isActive
      players <- DBEffect.getPlayersByPos(roomId)(positions*)
      _ <- DBQuery.playersByPos(roomId)(positions*).delete
      _ <- PlayerTable.players ++= players.map(_.copy (
        hasResigned = resigned,
        hasOfferedDraw = false,
      ))
      nonResigned <- DBEffect.getAllPlayers(roomId).map(_.filter(!_.hasResigned))
      _ <- if nonResigned.sizeIs == 1
        then DBQuery.room(roomId).map(_.status).update(Status.Complete)
        else DBIO.successful(())
    yield ()
    db.run(action.transactionally)
    
  def offerDraw (roomId: String, draw: Boolean = true) (positions: PlayerId*): Future[Unit] =
    val action = for
      room <- DBEffect.getRoom(roomId)
      if room.isActive
      players <- DBEffect.getPlayersByPos(roomId)(positions*).map(_.filter(!_.hasResigned))
      _ <- DBQuery.playersByPos(roomId)(positions*).delete
      _ <- PlayerTable.players ++= players.map(_.copy (
        hasResigned = false,
        hasOfferedDraw = draw,
      ))
      active <- DBEffect.getAllPlayers(roomId).map(_.filter(p => !p.hasOfferedDraw && !p.hasResigned))
      _ <- if active.isEmpty
        then DBQuery.room(roomId).map(_.status).update(Status.Complete)
        else DBIO.successful(())
    yield ()
    db.run(action.transactionally)
    
  def getGameState (roomId: String): Future[GameState] =
    val action = for
      room <- DBEffect.getRoom(roomId)
      numPlayers <- DBEffect.getNumPlayers(roomId)
      inputs <- DBEffect.getInputHistory(roomId)
      requiredPlayers <- room.forkedFrom match
        case Some(forkedFrom) => DBEffect.getNumPlayers(forkedFrom)
        case None => DBIOAction.successful(room.game.numPlayers.filter(_ >= numPlayers).min)
      initial = room.game.initial(Game.GameConfig(requiredPlayers, room.properties, room.seed))
      current = inputs.foldLeft[GameState](initial): (state, action) =>
        state.applyInputById(action.inputId).get
    yield current
    db.run(action).recover{e => e.printStackTrace(); ???}
    
  private[models] object DBEffect:
    
    def getAllPlayers (roomId: String): DBIO[Seq[PlayerRow]] =
      DBQuery.allPlayers(roomId).result
      
    def getNumPlayers (roomId: String): DBIO[Int] =
      getAllPlayers(roomId).map(_.size)
    
    def getPlayersByUser (roomId: String, userId: Int): DBIO[Seq[PlayerRow]] =
      DBQuery.playersByUser(roomId, userId).result
      
    def getPlayerByPos (roomId: String, position: PlayerId): DBIO[PlayerRow] =
      getPlayerByPosOption(roomId, position).map(_.get)
      
    def getPlayerByPosOption (roomId: String, position: PlayerId): DBIO[Option[PlayerRow]] =
      DBQuery.playersByPos(roomId)(position).result.headOption
      
    def getPlayersByPos (roomId: String) (positions: PlayerId*): DBIO[Seq[PlayerRow]] =
      DBQuery.playersByPos(roomId)(positions*).result
      
    def getRoom (roomId: String): DBIO[Room] =
      getRoomOption(roomId).map(_.get)
      
    def getRoomOption (roomId: String): DBIO[Option[Room]] =
     DBQuery.room(roomId).result.headOption
      
    def getInputHistory (roomId: String): DBIO[Seq[InputRow]] =
      DBQuery.inputs(roomId).result
      
  private[models] object DBQuery:
    
    def allPlayers (roomId: String): Query[PlayerTable, PlayerRow, Seq] =
      PlayerTable.players
        .filter(_.roomId === roomId)
        .sortBy(_.position)
    
    def playersByUser (roomId: String, userId: Int): Query[PlayerTable, PlayerRow, Seq] =
      allPlayers(roomId).filter(_.userId === userId)
      
    def players (roomId: String) (userIds: Int*): Query[PlayerTable, PlayerRow, Seq] =
      allPlayers(roomId).filter(_.userId.inSet(userIds))
      
    def playersByPos (roomId: String) (positions: PlayerId*): Query[PlayerTable, PlayerRow, Seq] =
      allPlayers(roomId).filter(_.position.inSet(positions.map(_.toInt)))
      
    def playersBefore (roomId: String, position: PlayerId): Query[PlayerTable, PlayerRow, Seq] =
      allPlayers(roomId).filter(_.position < position.toInt)
      
    def playersAfter (roomId: String, position: PlayerId): Query[PlayerTable, PlayerRow, Seq] =
      allPlayers(roomId).filter(_.position > position.toInt)
      
    def inputs (roomId: String): Query[InputTable, InputRow, Seq] =
      InputTable.inputs.filter(_.roomId === roomId).sortBy(_.turnId)
      
    def room (roomId: String): Query[RoomTable, Room, Seq] =
      RoomTable.rooms.filter(_.id === roomId)
    
  private def generateRoomId (length: Int = 5): String =
    Random.between(0, 1 << (4 * length)).toHexString.toUpperCase.padTo(length, '0')