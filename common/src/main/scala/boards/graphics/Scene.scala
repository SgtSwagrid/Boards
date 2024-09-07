package boards.graphics

import boards.algebra.GameState
import boards.algebra.Action.*
import boards.algebra.PieceSet.Diff
import boards.graphics.Scene.*
import boards.graphics.{Colour, Scene, Shape, Texture}
import boards.protocol.GameProtocol.*
import util.math.Vec
import util.math.Vec.VecI
import io.circe.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import io.circe.parser.*
import util.math.kernel.Kernel

case class Scene (
  board: Kernel[Tile] = Kernel.empty,
  players: Seq[Player] = Seq.empty,
)

object Scene:
  
  case class Tile (
    pos: VecI,
    shape: Shape = Shape.Rectangle(1, 1),
    colour: Colour = Colour.White,
    piece: Option[Texture] = None,
    inputs: Seq[Input] = Seq.empty,
  )
  
  case class Input (
    inputType: InputType,
    pos: VecI,
    actionHash: Int,
    diff: Seq[Diff],
  )
  
  enum InputType:
    case Click(pos: VecI)
    case Drag(from: VecI, to: VecI)
    case None
  
  def apply(state: GameState, players: Seq[Player]): Scene =
    
    val inputs: Map[VecI, Seq[Input]] = state.next.map: successor =>
      val (pos, inputType) = successor.action match
        case Place(_, _, pos) => (pos, InputType.Click(pos))
        case Move(_, from, to) => (from, InputType.Drag(from, to))
        case Destroy(piece) => (piece.position, InputType.Click(piece.position))
        case NoOp => (VecI.zero, InputType.None)
      Input(inputType, pos, successor.action.hashCode(), successor.diff)
    .toSeq.groupBy(_.pos)
    
    new Scene (
      state.now.board.paint: (pos, colour) =>
        Tile (
          pos,
          Shape.Rectangle(1, 1),
          state.now.board.label(pos).get,
          state.now.pieces.get(pos).map(_.texture),
          inputs.getOrElse(pos, Seq.empty),
        ),
    )
    
  def empty: Scene = new Scene()