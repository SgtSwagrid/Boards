package boards.dsl.rules

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.dsl.Shortcuts.{*, given}
import boards.math.region.Region.HasRegionI

/** A means for determining what kinds of [[Input]] are currently possible.
  * The key subtype of [[Capability]] is the [[GameState]].
  */
trait Capability:
  
  /** The set of all successor states which follow this state.
    *
    * A successor state is any state that could ''possibly'' be reached
    * from this state immediately after a ''single'' [[Input]] is taken by the user.
    *
    * Successors are generated lazily as needed,
    * and the result is completely and permanently cached for future reference.
    */
  lazy val next: LazyList[GameState]
  
  /** The set of all legal [[Input]]s that the player currently may perform. */
  lazy val inputs: LazyList[Input] =
    next.flatMap(_.latestInput)
  
  /** Determine whether it is possible to perform an [[Input]]
    * such that a predicate holds for the resulting state.
    *
    * This method can be quite expensive, as it simply enumerates all
    * successor states and checks the predicate for each one.
    */
  def can(f: HistoryState ?=> Boolean): Boolean =
    next.exists(s => f(using s.history))
    
  /** Whether it is currently possible to perform any [[Input]]. */
  def canAct: Boolean =
    can(true)
    
  /** Whether it is currently possible to move any [[Piece]]. */
  def canMove: Boolean =
    can(Pieces.now.sincePrevious.hasMoved)
    
  /** Whether it is currently possible to move any [[Piece]] to a particular `region`. */
  def canMoveTo(region: HasRegionI): Boolean =
    can(Pieces.now.sincePrevious.hasMovedTo(region))
  
  /** Whether it is currently possible to move any [[Piece]] out of a particular `region`. */
  def canMoveFrom(region: HasRegionI): Boolean =
    can(Pieces.now.sincePrevious.hasMovedFrom(region))
    
  /** Whether it is currently possible to capture any of the given [[Piece]]s by moving on top of them. */
  def canCapture(pieces: PieceFilter): Boolean = can:
    Pieces.now.sincePrevious.hasMovedTo(pieces.atPrevious)
    
  /** Whether it is currently possible to click any position on the [[Board]]. */
  def canClick: Boolean = can:
    State.latestInput.exists:
      case Input.Click(_) => true
      case _ => false
    
  /** Whether it is currently possible to click any position in the `region`. */
  def canClick(region: RegionI): Boolean = can:
    State.latestInput.exists:
      case Input.Click(clicked) => (clicked & region).nonEmpty
      case _ => false
      
  /** Whether it is currently possible to click any of the given [[Piece]]s. */
  def canClick(pieces: PieceFilter): Boolean = can:
    State.latestInput.exists:
      case Input.Click(clicked) => (clicked & pieces.atPrevious).nonEmpty
      case _ => false
      
  /** Whether it is currently possible to perform a [[Drag]]. */
  def canDrag: Boolean = can:
    State.latestInput.exists:
      case Input.Drag(_, _) => true
      case _ => false