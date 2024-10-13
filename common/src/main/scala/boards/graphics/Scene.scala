package boards.graphics

import boards.Games
import boards.protocol.GameProtocol.*
import boards.graphics.Scene.*
import boards.imports.circe.{*, given}
import boards.imports.math.{*, given}
import boards.imports.games.{*, given}

case class Scene (
  room: Room = Room.empty,
  participant: Participant = Unregistered,
  board: Kernel[Tile] = Kernel.empty,
  pieces: Seq[PieceData] = Seq.empty,
  inputs: Seq[Input] = Seq.empty,
  diff: Seq[VecI] = Seq.empty,
  players: Seq[Player] = Seq.empty,
  activePlayerId: Int = 0,
) derives Codec.AsObject:
  lazy val inputsByOrigin = inputs.groupBy(_.from)
  lazy val piecesById = pieces.map(p => p.pieceId -> p).toMap
  lazy val piecesByPos = pieces.map(p => p.pos -> p).toMap
  lazy val diffSet = diff.toSet
  def activePlayer = players(activePlayerId)
  def activePlayerName = game.playerNames(activePlayerId)
  def activePlayerColour = game.playerColours(activePlayerId)
  export room.{game, status}

object Scene:
  import Scene.*
  
  case class Tile (
    pos: VecI,
    shape: Shape = Shape.Rectangle(1, 1),
    colour: Colour = Colour.White,
    piece: Option[Texture] = None,
  ) derives Codec.AsObject
  
  case class Input (
    from: VecI,
    to: VecI,
    actionHash: String,
    result: Scene,
  ) derives Codec.AsObject
  
  case class PieceData (
    pieceId: Int,
    pos: VecI,
    texture: Texture,
  ) derives Codec.AsObject
  
  def apply(state: GameState, players: Seq[Player], room: Room, spectator: Participant): Scene =
    
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
      if !room.status.isActive || !spectator.isActivePlayer(state.activePlayer.toInt) then Seq() else
        state.next.toSeq.map: successor =>
          val (from, to) = successor.action match
            case Place(_, _, pos) => (pos, pos)
            case Move(_, from, to) => (from, to)
            case Destroy(piece) => (piece.position, piece.position)
            case NoOp => throw new IllegalStateException
          val result = Scene(successor.inert, players, room, spectator)
          Input(from, to, successor.action.hash, result)
    
    val inCheck = boards.games.Chess.inCheck(using state)
    
    new Scene (
      if state.isFinal then room.copy(status = Status.Complete) else room,
      spectator,
      board,
      pieces,
      inputs,
      state.turnDiff.toSeq,
      players,
      state.activePlayer.toInt
    )
    
  def empty: Scene = new Scene()