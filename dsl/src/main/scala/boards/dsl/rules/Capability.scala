package boards.dsl.rules

import boards.imports.games.{*, given}
import boards.math.region.Region.{HasRegionI, RegionI}
import boards.dsl.Shortcuts.{*, given}

trait Capability:
  
  lazy val next: LazyList[GameState]
  
  lazy val inputs: LazyList[Input] =
    next.flatMap(_.latestInput)
  
  def can(f: HistoryState ?=> Boolean): Boolean =
    next.exists(s => f(using s.history))
    
  def canAct: Boolean =
    can(true)
    
  def canMove: Boolean =
    can(Pieces.now.sincePrevious.hasMoved)
    
  def canMoveTo(region: HasRegionI): Boolean =
    can(Pieces.now.sincePrevious.hasMovedTo(region))
    
  def canMoveFrom(region: HasRegionI): Boolean =
    can(Pieces.now.sincePrevious.hasMovedFrom(region))
    
  def canCapture(pieces: PieceFilter): Boolean = can:
    Pieces.now.sincePrevious.hasMovedTo(pieces.atPrevious)
    
  def canClick: Boolean = can:
    State.latestInput.exists:
      case Input.Click(_) => true
      case _ => false
    
  def canClick(region: RegionI): Boolean = can:
    State.latestInput.exists:
      case Input.Click(clicked) => (clicked & region).nonEmpty
      case _ => false
      
  def canClick(pieces: PieceFilter): Boolean = can:
    State.latestInput.exists:
      case Input.Click(clicked) => (clicked & pieces.atPrevious).nonEmpty
      case _ => false
      
  def canDrag: Boolean = can:
    State.latestInput.exists:
      case Input.Drag(_, _) => true
      case _ => false