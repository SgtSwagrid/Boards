package boards.graphics

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.imports.circe.{*, given}
import boards.protocol.GameProtocol.*
import boards.protocol.UserProtocol.*
import boards.protocol.Room
import boards.protocol.Room.*
import Scene.*

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
 *
 * @param user The user who is currently logged in on this device.
 * @param activePlayerId The ID of the player whose turn it currently is.
 *
 * @param board The set of empty tiles that forms the game board.
 * @param pieces The pieces which currently exist on the board.
 * @param inputs The set of legally permissible inputs for this user in the current state.
 * @param diff The tiles which were modified during the previous turn.
 * @param outcome The result of the game, if it has ended.
 */
case class Scene (
  
  room: RichRoom = Room.empty,
  
  user: Option[User] = None,
  activePlayerId: PlayerId = PlayerId(0),
  
  board: Kernel[Tile] = Kernel.empty,
  pieces: Seq[PieceData] = Seq.empty,
  inputs: Seq[Input] = Seq.empty,
  diff: Seq[VecI] = Seq.empty,
  outcome: Option[Outcome] = None,
  
) derives Codec.AsObject:
  
  /** All inputs indexed by starting board position. */
  lazy val inputsByOrigin: Map[VecI, Seq[Input]] = inputs.groupBy(_.from)
  /** All pieces indexed by piece ID. */
  lazy val piecesById: Map[Int, PieceData] = pieces.map(p => p.pieceId -> p).toMap
  /** All pieces indexed by board position. */
  lazy val piecesByPos: Map[VecI, PieceData] = pieces.map(p => p.position -> p).toMap
  /** A set of the tiles which were modified during the previous turn. */
  lazy val diffSet: Set[VecI] = diff.toSet
  
  /** Whether this participant is currently signed in. */
  lazy val iAmRegistered: Boolean = user.isDefined
  
  /** The player whose turn it currently is. */
  lazy val activePlayer: RichPlayer = players(activePlayerId)
  
  /** All the players that are playing on this device. */
  lazy val myPlayers: Seq[RichPlayer] =
    user.toSeq.flatMap(user => playersByUser.get(user.userId).toSeq.flatten)
  
  /** Whether at least one player is playing on this device. */
  lazy val iAmPlaying: Boolean = myPlayers.sizeIs > 0
  /** Whether exactly one player is playing on this device. */
  lazy val iAmPlayingAlone: Boolean = myPlayers.sizeIs == 1
  /** Whether more than one player is playing on this device. */
  lazy val iAmPlayingHotseat: Boolean = myPlayers.sizeIs > 1
  
  /** Whether the given player is controlled on this device. */
  def isMe(playerId: PlayerId): Boolean = myPlayers.exists(_.position == playerId)
  
  /** Whether it is the turn of a player on this device. */
  lazy val isMyTurn: Boolean = user.exists(user => activePlayer.userId == user.userId)
  /** Whether it is the turn of the only player on this device. */
  lazy val isMyTurnAlone: Boolean = isMyTurn && !iAmPlayingHotseat
  
  /** The winner of the game, if there is one. */
  lazy val winner: Option[RichPlayer] =
    (outcome match
      case Some(Winner(winner)) => Some(players(winner))
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
    winner.exists(winner => user.exists(user => winner.userId == user.userId))
  /** Whether the only player on this device won the game. */
  lazy val iWonAlone: Boolean = iWon && iAmPlayingAlone
    
  /** Whether a player on another device won the game. */
  lazy val iLost: Boolean =
    user.isDefined && winner.exists(winner => user.forall(user => winner.userId != user.userId))
  /** Whether the only player on this device lost the game. */
  lazy val iLostAlone: Boolean = iLost && iAmPlayingAlone
  
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
  case class Input (
    from: VecI,
    to: VecI,
    actionHash: String,
    result: Scene,
  ) derives Codec.AsObject
  
  /**
   * A visual representation of a piece on the game board.
   *
   * @param pieceId A unique code used to identify this piece.
   * @param position The logical position of this piece on the game board.
   * @param texture The current texture of this piece.
   */
  case class PieceData (
    pieceId: Int,
    position: VecI,
    texture: Texture,
  ) derives Codec.AsObject
  
  /**
   * Constructs a new scene from an actual state.
   * (This is used by the server to produce a scene before sending it to the client.)
   *
   * @param room The room in which the game is being played.
   * @param user The user for whom the scene is to be rendered.
   * @param state The actual game state.
   */
  def apply (
    room: RichRoom,
    user: Option[User],
    state: GameState,
  ): Scene =
    
    val board = state.now.board.paint: (pos, colour) =>
      Tile (
        pos,
        Shape.Rectangle(1, 1),
        state.now.board.label(pos).get,
        state.now.pieces.get(pos).map(_.texture),
      )
      
    val pieces = state.now.pieces.pieces.toSeq.map: piece =>
      PieceData(piece.id, piece.position, piece.texture)
    
    val inputs =
      val isMyTurn = room.isActive &&
        user.exists(user => room.players(state.activePlayer).userId == user.userId) &&
        !room.players(state.activePlayer).hasResigned
      if !isMyTurn then Seq.empty else
        state.next.toSeq.map: successor =>
          val (from, to) = successor.actionOption.get match
            case Place(_, _, pos) => (pos, pos)
            case Move(_, from, to) => (from, to)
            case Destroy(piece) => (piece.position, piece.position)
            case Skip => throw new IllegalStateException
          val result = Scene(room, user, successor.inert)
          Input(from, to, successor.actionOption.get.hash, result)
    
    new Scene (
      room = room,
      user = user,
      activePlayerId = state.activePlayer,
      board = board,
      pieces = pieces,
      inputs = inputs,
      diff = state.turnDiff.toSeq,
      outcome = state.outcomeOption,
    )
    
  /** An empty scene without any board or players. */
  def empty: Scene = new Scene()