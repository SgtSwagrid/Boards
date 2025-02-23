package boards.dsl.states

import boards.dsl.meta.PlayerRef.PlayerId
import boards.dsl.rules.{Capability, Cause, Effect, Input, Rule}
import boards.dsl.states.GameState.{ActiveState, FinalState, Outcome}

/** The combination of a [[HistoryState]] (the entire history of states in a game)
  * with a [[Rule]] (the mechanism for testing input legality and deciding what happens next).
  * In this way, we fully describe the past, present and future of this state.
  *
  * The [[Rule]] is not a direct property of a game. Each subsequence [[GameState]] can have a new [[Rule]].
  * As such, the rules of the game can (and usually will) change dynamically over time.
  *
  * @author Alec Dorrington
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
    inert,
  }
  
  lazy val next: LazyList[GameState] =
    ruleOption.map(_.next(history)).getOrElse(LazyList.empty)
    
  /** Apply any immediate non-ambiguous [[Effect]]s to this state
    * that don't require an [[Input]] to first be performed.
    *
    * Used internally to automatically apply effects after each [[Input]]
    * and also perform setup before the [[Game]] starts.
    *
    * This operation is idempotent and has no effect if there are no outstanding [[Effect]]s.
    */
  private[dsl] inline def applyEffect: GameState =
    ruleOption
      .filter(_ != Effect.identity)
      .flatMap(_.effect(history))
      .getOrElse(this)
  
  /** Whether the [[Game]] has become blocked in this [[GameState]], meaning that:
    *
    * (a) there are no legal [[Input]]s, and
    *
    * (b) the current [[Rule]] also cannot be skipped.
    */
  def isBlocked: Boolean =
    ruleOption.exists(_.successors(history).isEmpty)
  
  /** The [[Rule]] used to generate successor [[GameState]]s from this one, unless the game has ended. */
  def ruleOption: Option[Rule] = this match
    case ActiveState(_, rule) => Some(rule)
    case FinalState(_, _) => None
    
  /** If the game hasn't yet ended, replace the [[Rule]] with the one given. */
  def withRule(rule: Rule): GameState = this match
    case ActiveState(history, _) => ActiveState(history, rule)
    case state: FinalState => state
    
  /** If the game hasn't yet ended, update the [[Rule]] by the given function. */
  def updateRule(f: Rule => Rule): GameState = this match
    case ActiveState(history, rule) => ActiveState(history, f(rule))
    case state: FinalState => state
  
  /** The [[Outcome]] (who won?) of the game, if it has ended. */
  def outcomeOption: Option[Outcome] = this match
    case ActiveState(_, _) => None
    case FinalState(_, outcome) => Some(outcome)
    
  /** If the given [[Input]] is legal, get the resulting [[GameState]] of having taken it. */
  def applyInput(input: Input): Option[GameState] =
    next.find(_.latestInput.contains(input))
    
  /** Get the result of applying the `inputId`-th legal [[Input]] to the current [[GameState]]. */
  def applyInputById(inputId: Int): Option[GameState] =
    Option.when(inputId >= 0 && next.sizeIs > inputId)(next(inputId))
    
  /** Whether this is a final state, meaning the game has ended. */
  def isFinal: Boolean = this match
    case ActiveState(_, _) => false
    case FinalState(_, _) => true
  
  override def toString = history.toString

object GameState:
  
  /** The initial [[GameState]] for a new [[Game]] instance. */
  def initial(state: HistoryState, rule: Rule): GameState =
    rule.from(state)
    
  /** An empty [[GameState]] for use as a placeholder. */
  def empty: GameState =
    HistoryState.empty.withRule(Cause.none)
  
  /**
   * A [[GameState]] for a situation where the game hasn't yet ended.
   * @param history All past and present states of this game instance.
   * @param rule The [[Rule]] used to generate successor [[GameState]]s from this one.
   */
  case class ActiveState (
    history: HistoryState,
    rule: Rule,
  ) extends GameState
  
  /**
   * A [[GameState]] for a situation where the game has already ended.
   * @param history All past and present states of this game instance.
   * @param outcome The [[Outcome]] (who won?) of the game.
   */
  case class FinalState (
    history: HistoryState,
    outcome: Outcome,
  ) extends GameState
  
  /** A flag describing the [[Outcome]] of the game, i.e. who won or was it a draw? */
  enum Outcome:
    
    /** Indicates that the game resulted in this `Player` winning. */
    case Winner(winner: PlayerId)
    /** Indicates that the game resulted in a draw. */
    case Draw
    
    /** The [[Effect]] of ending the [[Game]] with this [[Outcome]]. */
    def declare: Effect = Effect.stop(this)