package boards.graphics

import boards.protocol.GameProtocol.*
import boards.graphics.Scene.*
import boards.imports.circe.{*, given}
import boards.imports.math.{*, given}
import boards.imports.games.{*, given}

case class Scene (
  board: Kernel[Tile] = Kernel.empty,
  pieces: Seq[PieceData] = Seq.empty,
  inputs: Seq[Input] = Seq.empty,
  players: Seq[Player] = Seq.empty,
) derives Codec.AsObject:
  lazy val inputsByOrigin = inputs.groupBy(_.from)
  lazy val piecesById = pieces.map(p => p.pieceId -> p).toMap
  lazy val piecesByPos = pieces.map(p => p.pos -> p).toMap

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
    actionHash: Int,
    result: Scene,
  ) derives Codec.AsObject
  
  case class PieceData (
    pieceId: Int,
    pos: VecI,
    texture: Texture,
  ) derives Codec.AsObject
  
  def apply(state: GameState, players: Seq[Player]): Scene =
    
    val board = state.now.board.paint: (pos, colour) =>
      Tile (
        pos,
        Shape.Rectangle(1, 1),
        state.now.board.label(pos).get,
        state.now.pieces.get(pos).map(_.texture),
      )
      
    val pieces = state.now.pieces.pieces.toSeq.map: piece =>
      PieceData(piece.id, piece.position, piece.texture)
    
    val inputs = state.next.toSeq.map: successor =>
      val (from, to) = successor.action match
        case Place(_, _, pos) => (pos, pos)
        case Move(_, from, to) => (from, to)
        case Destroy(piece) => (piece.position, piece.position)
        case NoOp => throw new IllegalStateException
      val result = Scene(successor.inert, players)
      Input(from, to, successor.action.hashCode(), result)
    
    new Scene(board, pieces.toSeq, inputs, players)
    
  def empty: Scene = new Scene()