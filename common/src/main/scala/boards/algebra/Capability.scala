package boards.algebra

import boards.GameImports.{*, given}
import boards.algebra.Generator.*

class Capability (
  rule: Rule,
  state: GameState
):
  
  given GameState = state
  
  private def isPossible(f: PartialFunction[Generator, Rule]): Boolean =
    state match
      case _: FinalState => false
      case state: NonFinalState =>
        rule.updateGenerators(f).actions(state).exists(_ != NoOp)
  
  def isLegal(action: Action): Boolean = action match
    case Place(owner, piece, pos) => isPossible:
      case PlaceGenerator(player, pieces, kernel)
        if player == owner && kernel.contains(pos) && pieces.contains(piece) =>
          Generator.place(player)(pos -> piece)
    case Move(_, from, to) => isPossible:
      case MoveGenerator(fromKernel, toKernel)
        if fromKernel.contains(from) && toKernel.contains(to) =>
          Generator.move(from -> to)
    case Destroy(piece) => isPossible:
      case DestroyGenerator(kernel)
        if kernel.contains(piece) =>
          Generator.destroy(piece)
  
  def canAct: Boolean = isPossible(g => g)
  
  def canPlace: Boolean = isPossible:
    case m: PlaceGenerator => m
  
  def canMove: Boolean = isPossible:
    case m: MoveGenerator => m
  
  def canDestroy: Boolean = isPossible:
    case m: DestroyGenerator => m
  
  def canPlaceAt(region: Kernel[?]): Boolean = isPossible:
    case PlaceGenerator(owner, pieces, kernel) =>
      PlaceGenerator(owner, pieces, kernel & region)
  
  def canMoveFrom(region: Kernel[?]): Boolean = isPossible:
    case MoveGenerator(from, to) =>
      MoveGenerator(from & region, to)
      
  def canMoveTo(region: Kernel[?]): Boolean = isPossible:
    case MoveGenerator(from, to) =>
      MoveGenerator(from, to & region)
  
  def canDestroy(region: Kernel[?]): Boolean = isPossible:
    case DestroyGenerator(kernel) =>
      DestroyGenerator(kernel & region)
      
  def canAttack(region: Kernel[?], piece: Piece | None.type = None): Boolean =
    piece match
      case piece: Piece =>
        Capability(rule, state.place(piece.owner)(region -> piece.state)).canAttack(region)
      case None => isPossible:
        case PlaceGenerator(owner, pieces, kernel) => PlaceGenerator(owner, pieces, kernel & region)
        case MoveGenerator(from, to) => MoveGenerator(from, to & region)
        case DestroyGenerator(kernel) => DestroyGenerator(kernel & region)
    
  def isOptional: Boolean = state match
    case _: FinalState => false
    case state: NonFinalState =>
      rule.updateGenerators:
        case SkipGenerator => Generator.skip
      .actions(state).nonEmpty