package boards.graphics

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.imports.circe.{*, given}
import boards.protocol.GameProtocol.*
import boards.protocol.UserProtocol.*
import boards.protocol.Room
import boards.protocol.Room.*
import Scene.*
import boards.dsl.pieces.PieceState.empty.region
import boards.dsl.rules
import boards.dsl.states.GameState
import boards.math.region.RegionMap.RegionMapI
import boards.math.region.{Region, RegionMap}
import io.circe.Decoder.state

/**
 * A representation of the current game state.
 * This is only a shallow image and cannot be used to reconstruct the actual state.
 * This is for use primarily as a data exchange format, so that the server may
 * provide the client with precisely that information necessary to render the current state
 * and accept appropriate input from the user;
 * in a manner that is automatically serialisable into JSON
 * and without revealing hidden information that should remain secret.
 *
 * @param room The game room currently being viewed.
 * @param userId The ID of the user who is currently logged in on this device.
 * @param activePlayerId The ID of the player whose turn it currently is.
 * @param board The set of empty tiles that forms the game board.
 * @param pieces The pieces which currently exist on the board.
 * @param choices The set of legally permissible inputs for this user in the current state.
 * @param diff The tiles which were modified during the previous turn.
 * @param logicalOutcome The result of the game, if it has ended.
 * @param time The number of actions which have already been taken.
 */
case class Scene (
  
  room: RichRoom = Room.empty,
  
  userId: Option[Int] = None,
  activePlayerId: PlayerId = PlayerId(0),
  
  board: RegionMapI[Tile] = RegionMap.empty,
  pieces: Seq[PieceData] = Seq.empty,
  choices: Seq[Choice[Input]] = Seq.empty,
  diff: RegionI = Region.empty,
  logicalOutcome: Option[Outcome] = None,
  currentTurnId: TurnId = TurnId.initial,
  latestTurnId: TurnId = TurnId.initial,
  
) derives Codec.AsObject:
  
  lazy val (clicks, drags) = choices.partitionMap:
    case Choice(click: Input.Click, result, id) => Left(Choice(click, result, id))
    case Choice(drag: Input.Drag, result, id) => Right(Choice(drag, result, id))
    
  lazy val clicksByOrigin: Map[VecI, Seq[Choice[Input.Click]]] =
    clicks.flatMap: choice =>
      choice.input.from.positions.map: pos =>
        pos -> choice
    .groupBy((pos, _) => pos)
    .map: (pos, choices) =>
      pos -> choices.map((pos, choice) => choice)
  
  lazy val dragsByOrigin: Map[VecI, Seq[Choice[Input.Drag]]] =
    drags.flatMap: choice =>
      choice.input.from.positions.map: pos =>
        pos -> choice
    .groupBy((pos, _) => pos)
    .map: (pos, choices) =>
      pos -> choices.map((pos, choice) => choice)
    
  /** All pieces indexed by piece ID. */
  lazy val piecesById: Map[PieceId, PieceData] = pieces.map(p => p.pieceId -> p).toMap
  /** All pieces indexed by board position. */
  lazy val piecesByPos: Map[VecI, PieceData] = pieces.map(p => p.position -> p).toMap
  
  /** Whether this participant is currently signed in. */
  def iAmRegistered: Boolean = userId.isDefined
  
  /** The player whose turn it currently is. */
  def activePlayer: RichPlayer = player(activePlayerId)
  
  /** All the players that are playing on this device. */
  lazy val myPlayers: Seq[RichPlayer] =
    userId.toSeq.flatMap(userId => playersByUser.get(userId).toSeq.flatten)
  
  /** Whether at least one player is playing on this device. */
  def iAmPlaying: Boolean = myPlayers.sizeIs > 0
  /** Whether exactly one player is playing on this device. */
  def iAmPlayingAlone: Boolean = myPlayers.sizeIs == 1
  /** Whether more than one player is playing on this device. */
  def iAmPlayingHotseat: Boolean = myPlayers.sizeIs > 1
  /** Whether all players are playing on this device. */
  def iAmPlayingExclusivelyHotseat: Boolean = isExclusivelyHotseat && iAmPlaying
  
  /** Whether the given player is controlled on this device. */
  def isMe(playerId: PlayerId): Boolean = myPlayers.exists(_.position == playerId)
  
  /** Whether it is the turn of a player on this device. */
  def isMyTurn: Boolean = userId.contains(activePlayer.userId)
  /** Whether it is the turn of the only player on this device. */
  def isMyTurnAlone: Boolean = isMyTurn && !iAmPlayingHotseat
  
  def isInitialState: Boolean = currentTurnId.isInitial
  def isLatestState: Boolean = currentTurnId == latestTurnId
  
  def isActiveHere: Boolean =
    isActive || (isComplete && !isLatestState)
  
  def outcome: Option[Outcome] =
    logicalOutcome.orElse(agreedOutcome)
  
  /** The winner of the game, if there is one. */
  lazy val winner: Option[RichPlayer] =
    (logicalOutcome match
      case Some(Winner(winner)) => Some(player(winner))
      case _ => None
    ).orElse(winnerByResignation)
  /** Whether the given player has won the game. */
  def isWinner(playerId: PlayerId): Boolean = winner.exists(_.position == playerId)
  
  /** Whether the game has a winner. */
  def hasWinner: Boolean = winner.isDefined
  /** Whether the game was a draw. */
  def isDraw: Boolean = isComplete && !hasWinner
  
  
  def iWon: Boolean =
    winner.exists(winner => userId.contains(winner.userId))
  /** Whether the only player on this device won the game. */
  def iWonAlone: Boolean = iWon && iAmPlayingAlone
  
  /** Whether a player on another device won the game. */
  def iLost: Boolean =
    userId.isDefined && winner.exists(winner => !userId.contains(winner.userId))
  /** Whether the only player on this device lost the game. */
  def iLostAlone: Boolean = iLost && iAmPlayingAlone
  
  lazy val (activePlayers, resignedPlayers, drawnPlayers) =
    val (active, inactive) =  players.partition(p => !p.hasResigned && !p.hasOfferedDraw)
    val (resigned, drawn) = inactive.partition(_.hasResigned)
    (active, resigned, drawn)
  
  lazy val (myActivePlayers, myResignedPlayers, myDrawnPlayers) =
    val (active, inactive) = myPlayers.partition(p => !p.hasResigned && !p.hasOfferedDraw)
    val (resigned, drawn) = inactive.partition(_.hasResigned)
    (active, resigned, drawn)
  
  /** Whether any of the players on this device have resigned. */
  def iHaveResigned: Boolean = myResignedPlayers.nonEmpty
  /** Whether all the players on this device have resigned. */
  def iHaveResignedAll: Boolean = iHaveResigned && myActivePlayers.isEmpty && myDrawnPlayers.isEmpty
  /** Whether any of the players on this device have offered a draw. */
  def iHaveOfferedDraw: Boolean = myDrawnPlayers.nonEmpty
  /** Whether some player has offered a draw. */
  def someoneHasOfferedDraw: Boolean = drawnPlayers.nonEmpty
  
  def iCanOfferRematch: Boolean = iAmPlaying && rematch.isEmpty
  
  def iCanJoinRematch: Boolean = iAmPlaying && rematch.exists: rematch =>
    rematch.isPending && rematch.players.size < rematch.maxPlayers && !rematch.userIds.contains(userId.get)
  
  export room.*

object Scene:
  
  /**
   * A single tile of the game board.
   *
   * @param position The logical position of this tile on the game board.
   * @param shape The shape of this tile (i.e. square/triangle/hexagon).
   * @param colour The colour of this tile.
   * @param piece The texture of the piece which lies on this tile, if there is one.
   */
  case class Tile (
    position: VecI,
    shape: Shape = Shape.Rectangle(1, 1),
    colour: Colour = Colour.White,
    piece: Option[Texture] = None,
  ) derives Codec.AsObject
  
  /**
   * An input action that the user may choose to perform in the current state.
   *
   * @param from The position the user may drag from.
   * @param to The position the user may drag to (same as from for non-drag actions).
   * @param actionHash A unique code used to identify this action.
   * @param result The state that results from this action, so it can be displayed in realtime.
   */
  case class Choice[I <: Input] (
    input: I,
    result: Scene,
    choiceId: Int,
  ) derives Codec.AsObject:
    override def toString = input.toString
  
  /**
   * A visual representation of a piece on the game board.
   *
   * @param pieceId A unique code used to identify this piece.
   * @param position The logical position of this piece on the game board.
   * @param texture The current texture of this piece.
   */
  case class PieceData (
    pieceId: PieceId,
    position: VecI,
    texture: Texture,
  ) derives Codec.AsObject
  
  /**
   * Constructs a new scene from an actual state.
   * (This is used by the server to produce a scene before sending it to the client.)
   *
   * @param room The room in which the game is being played.
   * @param user The ID of the user for whom the scene is to be rendered.
   * @param state The actual game state.
   */
  def apply (
    room: RichRoom,
    userId: Option[Int],
    currentState: GameState,
    latestState: GameState,
  ): Scene =
    
    val board = currentState.now.board.zipWithPosition.mapLabels: (pos, colour) =>
      Tile (
        pos,
        Shape.Rectangle(1, 1),
        currentState.board.label(pos).get,
        currentState.pieces.at(pos).map(_.texture),
      )
      
    val pieces = currentState.now.pieces.pieces.map: piece =>
      PieceData(piece.pieceId, piece.position, piece.texture)
    
    val choices =
      val isMyTurn = room.isActive &&
        userId.contains(room.player(currentState.activePlayer).userId) &&
        !room.player(currentState.activePlayer).hasResigned
      if !isMyTurn then Seq.empty else
        currentState.next.zipWithIndex.map: (successor, id) =>
          Choice(successor.latestInput.get, Scene(room, userId, successor.inert, successor.inert), id)
    
    val diff =
      given HistoryState = currentState.history
      if currentState.history.isNewTurn
      then currentState.pieces.duringPreviousTurn.updatedRegion
      else currentState.pieces.sinceTurnStart.updatedRegion
    
    new Scene (
      room           = room,
      userId         = userId,
      activePlayerId = currentState.activePlayer,
      board          = board,
      pieces         = pieces,
      choices        = choices,
      diff           = diff,
      logicalOutcome = latestState.outcomeOption,
      currentTurnId  = currentState.turnId,
      latestTurnId   = latestState.turnId,
    )
    
  /** An empty scene without any board or players. */
  def empty: Scene = new Scene()