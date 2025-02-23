package boards.dsl.meta

import boards.util.extensions.CollectionOps.contramap

object TurnId:
  
  opaque type TurnId = Int
  
  extension (turnId: TurnId)
    def toInt: Int = turnId
    def + (x: Int | TurnId): TurnId = TurnId(Math.max(turnId + x, 0))
    def - (x: Int | TurnId): TurnId = TurnId(Math.max(turnId - x, 0))
    def % (d: Int | TurnId): TurnId = TurnId(turnId % d)
    def next: TurnId = turnId + 1
    def previous: TurnId = turnId - 1
    def isInitial: Boolean = turnId == 0
    
  def apply (turnId: Int): TurnId = turnId
  def initial: TurnId = TurnId(0)
  
  given Ordering[TurnId] = Ordering.Int.contramap(_.toInt)
  
  trait HasTurnId:
    val turnId: TurnId