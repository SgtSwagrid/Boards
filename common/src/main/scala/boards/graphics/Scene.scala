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
 * @param outcome The result of the game, if it has ended.
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
  outcome: Option[Outcome] = None,
  turnId: TurnId = TurnId.initial,
  
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
  lazy val iAmRegistered: Boolean = userId.isDefined
  
  /** The player whose turn it currently is. */
  lazy val activePlayer: RichPlayer = player(activePlayerId)
  
  /** All the players that are playing on this device. */
  lazy val myPlayers: Seq[RichPlayer] =
    userId.toSeq.flatMap(userId => playersByUser.get(userId).toSeq.flatten)
  
  /** Whether at least one player is playing on this device. */
  lazy val iAmPlaying: Boolean = myPlayers.sizeIs > 0
  /** Whether exactly one player is playing on this device. */
  lazy val iAmPlayingAlone: Boolean = myPlayers.sizeIs == 1
  /** Whether more than one player is playing on this device. */
  lazy val iAmPlayingHotseat: Boolean = myPlayers.sizeIs > 1
  
  /** Whether the given player is controlled on this device. */
  def isMe(playerId: PlayerId): Boolean = myPlayers.exists(_.position == playerId)
  
  /** Whether it is the turn of a player on this device. */
  lazy val isMyTurn: Boolean = userId.contains(activePlayer.userId)
  /** Whether it is the turn of the only player on this device. */
  lazy val isMyTurnAlone: Boolean = isMyTurn && !iAmPlayingHotseat
  
  /** The winner of the game, if there is one. */
  lazy val winner: Option[RichPlayer] =
    (outcome match
      case Some(Winner(winner)) => Some(player(winner))
      case _ => None
    ).orElse(winnerByResignation)
  /** Whether the given player has won the game. */
  def isWinner(playerId: PlayerId): Boolean = winner.exists(_.position == playerId)
    
  /** Whether the game has a winner. */
  lazy val hasWinner: Boolean = winner.isDefined
  /** Whether the game was a draw. */
  lazy val isDraw: Boolean = isComplete&& !hasWinner
  
  /** Whether a player on this device won the game. */
  lazy val iWon: Boolean =
    winner.exists(winner => userId.contains(winner.userId))
  /** Whether the only player on this device won the game. */
  lazy val iWonAlone: Boolean = iWon && iAmPlayingAlone
    
  /** Whether a player on another device won the game. */
  lazy val iLost: Boolean =
    userId.isDefined && winner.exists(winner => !userId.contains(winner.userId))
  /** Whether the only player on this device lost the game. */
  lazy val iLostAlone: Boolean = iLost && iAmPlayingAlone
  
  lazy val (activePlayers, resignedPlayers, drawnPlayers) =
    val (active, inactive) =  players.partition(p => !p.hasResigned && !p.hasOfferedDraw)
    val (resigned, drawn) = inactive.partition(_.hasResigned)
    (active, resigned, drawn)
  
  lazy val (myActivePlayers, myResignedPlayers, myDrawnPlayers) =
    val (active, inactive) = myPlayers.partition(p => !p.hasResigned && !p.hasOfferedDraw)
    val (resigned, drawn) = inactive.partition(_.hasResigned)
    (active, resigned, drawn)
  
  /** Whether any of the players on this device have resigned. */
  lazy val iHaveResigned: Boolean = myResignedPlayers.nonEmpty
  /** Whether all the players on this device have resigned. */
  lazy val iHaveResignedAll: Boolean = iHaveResigned && myActivePlayers.isEmpty && myDrawnPlayers.isEmpty
  /** Whether any of the players on this device have offered a draw. */
  lazy val iHaveOfferedDraw: Boolean = myDrawnPlayers.nonEmpty
  /** Whether some player has offered a draw. */
  lazy val someoneHasOfferedDraw: Boolean = drawnPlayers.nonEmpty
  
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
    state: GameState,
  ): Scene =
    
    val board = state.now.board.zipWithPosition.mapLabels: (pos, colour) =>
      Tile (
        pos,
        Shape.Rectangle(1, 1),
        state.now.board.label(pos).get,
        state.now.pieces.at(pos).map(_.texture),
      )
      
    val pieces = state.now.pieces.pieces.map: piece =>
      PieceData(piece.pieceId, piece.position, piece.texture)
    
    val choices =
      val isMyTurn = room.isActive &&
        userId.contains(room.player(state.activePlayer).userId) &&
        !room.player(state.activePlayer).hasResigned
      if !isMyTurn then Seq.empty else
        state.next.zipWithIndex.map: (successor, id) =>
          Choice(successor.latestInput.get, Scene(room, userId, successor.inert), id)
          
    val diff =
      given HistoryState = state.history
      if state.history.isNewTurn
      then state.pieces.duringPreviousTurn.updatedRegion
      else state.pieces.sinceTurnStart.updatedRegion
    
    new Scene (
      room           = room,
      userId         = userId,
      activePlayerId = state.activePlayer,
      board          = board,
      pieces         = pieces,
      choices        = choices,
      diff           = diff,
      outcome        = state.outcomeOption,
      turnId         = state.turnId,
    )
    
  /** An empty scene without any board or players. */
  def empty: Scene = new Scene()