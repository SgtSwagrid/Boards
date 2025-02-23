package boards.dsl.states

import boards.dsl.meta.TurnId.{next, HasTurnId, TurnId, given}
import boards.dsl.meta.TurnId
import boards.dsl.pieces.PieceState
import boards.dsl.pieces.PieceState.Version
import boards.dsl.rules.{Cause, Input, Rule}
import boards.dsl.states.GameState.{FinalState, Outcome}
import boards.dsl.states.HistoryState.{InitialState, SuccessorState}
import scala.math.Ordered.orderingToOrdered

/**
 * The entire history of states in a game.
 * Contains all [[InstantaneousState]]s and [[Input]]s to have occurred up to this point.
  *
  * @author Alec Dorrington
 */
sealed trait HistoryState extends HasTurnId:
  
  /** The most recent `InstantaneousState`. */
  val now: InstantaneousState
  
  val pieces: PieceState = now.pieces
  val version: Version = pieces.version
  
  export now.{
    board,
    config,
    activePlayer,
    nextPlayer,
    inactivePlayers,
    inBounds,
  }
  
  lazy val initial: HistoryState = this match
    case SuccessorState(_, previous, _) => previous.initial
    case InitialState(_) => this
  
  /** The previous `HistoryState`, without the most recent modification, if there is one.  */
  def previousOption: Option[HistoryState] = this match
    case SuccessorState(_, previous, _) => Some(previous)
    case _ => None
    
  def previousOrThis: HistoryState = previousOption.getOrElse(this)
    
  def turnStart: HistoryState = this match
    case SuccessorState(_, previous, _) if previous.activePlayer == activePlayer =>
      previous.turnStart
    case _ => this
    
  def previousTurnStart: HistoryState =
    turnStart.previousOrThis.turnStart
    
  def isNewTurn: Boolean = this match
    case SuccessorState(_, previous, _) => previous.activePlayer != activePlayer
    case InitialState(_) => true
    
  def when(condition: HistoryState => Boolean): Option[HistoryState] =
    if condition(this) then Some(this) else previousOption.flatMap(_.when(condition))
    
  def latestInput: Option[Input] = this match
    case SuccessorState(_, _, input) => Some(input)
    case _ => None
    
  def atTime(turnId: TurnId): HistoryState = this match
    case _ if turnId == this.turnId => this
    case state: SuccessorState if turnId < this.turnId => state.previous.atTime(turnId)
    case _ => HistoryState.empty
    
  /** Create a new `HistoryState` which directly follows this one after the `User` makes some `Input`. */
  def withCause(input: Input): SuccessorState =
    SuccessorState(now, this, input)
  
  /** Create a new `HistoryState` which directly follows this one after the `InstantaneousState` is modified. */
  def replace(state: InstantaneousState): HistoryState = this match
    case SuccessorState(_, previous, action) => SuccessorState(state, previous, action)
    case InitialState(_) => InitialState(state)
  
  def update(f: InstantaneousState => InstantaneousState): HistoryState = this match
    case SuccessorState(state, previous, action) => SuccessorState(f(state), previous, action)
    case InitialState(state) => InitialState(f(state))
    
  /** Pair this `HistoryState` with a `Rule` to create a `GameState`, so that this new `Rule` applies hereafter. */
  def withRule(rule: Rule): GameState =
    rule.from(this)
    
  def inert: GameState =
    withRule(Cause.none)
    
  /** Pair this `HistoryState` with an `Outcome` to indicate that the `Game` has ended. */
  def withOutcome(outcome: Outcome): FinalState =
    FinalState(this, outcome)

object HistoryState:
  
  def initial(state: InstantaneousState): HistoryState =
    InitialState(state)
    
  def empty: HistoryState = initial(InstantaneousState.empty)
  
  /**
   * A `HistoryState` for a situation where the `Game` has just started and there are no previous states.
   * @param now The initial `InstantaneousState` of the `Game`.
   */
  case class InitialState (
    now: InstantaneousState,
  ) extends HistoryState:
    val turnId: TurnId = TurnId.initial
    override def toString = now.toString
  
  /**
   * A `HistoryState` for the time directly following an `Input` from a `Player`.
   * @param previous The previous `HistoryState`, without the most recent modification.
   * @param cause The legal `Input` that was just performed by the `Player.`
   */
  case class SuccessorState (
    now: InstantaneousState,
    previous: HistoryState,
    cause: Input,
  ) extends HistoryState:
    val turnId: TurnId = previous.turnId.next
    override def toString = s"$previous |-($cause)-> $now"
    
  trait AtTime[X]:
    
    def atTime(state: HistoryState): X
    
    def now(using state: HistoryState): X =
      atTime(state)
    
    def atPrevious(using state: HistoryState): X =
      atTime(state.previousOrThis)
    
    def atTurnStart(using state: HistoryState): X =
      atTime(state.turnStart)
    
    def when(condition: HistoryState ?=> Boolean)(using state: HistoryState): X =
      atTime(state.when(state => condition(using state)).getOrElse(state.initial))
    
  trait PeriodQuery[X]:
    
    def between(start: HistoryState, end: HistoryState): X
    
    def since(start: HistoryState)(using end: HistoryState): X =
      between(start, end)
    
    def until(state: HistoryState): X =
      between(state.initial, state)
    
    def sincePrevious(using state: HistoryState): X =
      since(state.previousOrThis)
    
    def sinceTurnStart(using state: HistoryState): X =
      since(state.turnStart)
    
    def duringPreviousTurn(using state: HistoryState): X =
      between(state.previousTurnStart, state.turnStart)
    
    def sinceGameStart(using state: HistoryState): X =
      since(state.initial)