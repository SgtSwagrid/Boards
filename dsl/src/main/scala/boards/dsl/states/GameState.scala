package boards.dsl.states

import boards.dsl.rules.Capability
import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.math.region.Region.HasRegionI

/**
 * The combination of a `HistoryState` (the entire history of states in a game)
 * with a `Rule` (the mechanism for testing input legality and deciding what happens next).
 * In this way, we fully describe the past, present and future of this state.
 *
 * The `Rule` is not a direct property of a game. Each subsequence `GameState` can have a new `Rule`.
 * As such, the rules of the game can (and usually will) change dynamically over time.
 */
sealed trait GameState extends Capability:
  
  /** All past and present states of this game instance. */
  val history: HistoryState
  
  export history.{
    now,
    previousOption,
    latestInput,
    turnId,
    atTime,
    pieces,
    board,
    config,
    activePlayer,
    nextPlayer,
    inactivePlayers,
    inBounds,
  }
  
  lazy val successors: LazyList[GameState] =
    println(s"Calling successors from depth ${turnId}.")
    ruleOption.map(_.successors(history)).getOrElse(LazyList.empty)
    
  def effect: Option[GameState] =
    ruleOption.flatMap(_.effect(history))
    
  def applyEffect: GameState =
    effect.getOrElse(this)
  
  /** The `Rule` used to generate successor `GameState`s from this one, unless the game has ended. */
  def ruleOption: Option[Rule] = this match
    case ActiveState(_, rule) => Some(rule)
    case FinalState(_, _) => None
    
  /** If the game hasn't yet ended, replace the `Rule` with the one given. */
  def withRule(rule: Rule): GameState = this match
    case ActiveState(history, _) => ActiveState(history, rule)
    case state: FinalState => state
    
  def inert: GameState = withRule(Cause.none)
    
  /** If the game hasn't yet ended, update the `Rule` by the given function. */
  def updateRule(f: Rule => Rule): GameState = this match
    case ActiveState(history, rule) => ActiveState(history, f(rule))
    case state: FinalState => state
  
  /** The `Outcome` (who won?) of the game, if it has ended. */
  def outcomeOption: Option[Outcome] = this match
    case ActiveState(_, _) => None
    case FinalState(_, outcome) => Some(outcome)
    
  def applyInputById(inputId: Int): Option[GameState] =
    Option.when(inputId >= 0 && successors.sizeIs > inputId)(successors(inputId))
    
  def isFinal: Boolean = this match
    case ActiveState(_, _) => false
    case FinalState(_, _) => true
  
  override def toString = history.toString

object GameState:
  
  def initial(state: HistoryState, rule: Rule): GameState =
    ActiveState(state, rule).applyEffect
    
  def empty: GameState =
    HistoryState.empty.withRule(Cause.none)
  
  /**
   * A `GameState` for a situation where the game hasn't yet ended.
   * @param history All past and present states of this game instance.
   * @param rule The Rule used to generate successor `GameState`s from this one.
   */
  case class ActiveState (
    history: HistoryState,
    rule: Rule,
  ) extends GameState
  
  /**
   * A `GameState` for a situation where the game has already ended.
   * @param history All past and present states of this game instance.
   * @param outcome The Outcome (who won?) of the game.
   */
  case class FinalState (
    history: HistoryState,
    outcome: Outcome,
  ) extends GameState
  
  /** A flag describing the outcome of the game, i.e. who won or was it a draw? */
  enum Outcome:
    /** Indicates that the game resulted in this `Player` winning. */
    case Winner(winner: PlayerId)
    /** Indicates that the game resulted in a draw. */
    case Draw