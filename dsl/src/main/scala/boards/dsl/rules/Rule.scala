package boards.dsl.rules

import Rule.*
import boards.dsl.meta.PlayerRef.PlayerRef
import boards.dsl.states.{GameState, HistoryState}
import boards.dsl.meta.TurnId.next
import boards.dsl.states.GameState.{ActiveState, Outcome}
import boards.dsl.states.GameState.Outcome.Draw

import scala.annotation.targetName
import scala.collection.mutable

/** The mechanism for generating successor states and testing [[Input]] legality.
  * Conceptually, a [[Rule]] is a mapping from some [[HistoryState]]
  * to a set of [[GameState]]s that immediately follow.
  *
  * The most simple forms of [[Rule]] are [[Cause]]s and [[Effect]]s.
  * Respectively, these wait for [[Input]] from the user or update the [[InstantaneousState]].
  * More complex [[Rule]]s are built compositionally from these basic building blocks,
  * using combinators such as [[|]] and [[|>]].
  *
  * The rules of a [[boards.dsl.meta.Game]] are dynamic in that they may change after each [[Input]].
  * A [[boards.dsl.meta.Game]] doesn't have just a single corresponding [[Rule]].
  * Instead, an ''initial'' [[Rule]] is provided, and each [[Rule]]
  * thereafter is responsible for deciding, in each successor state, which other [[Rule]] should apply next.
  * This is why each [[GameState]] contains its own [[Rule]].
  * This detail need not concern the game implementer, as [[Rule]] combinators handle this succession internally.
  *
  * A [[Rule]] by itself doesn't store any state, but rather can act on ''any'' provided state.
  * This allows for a conceptual separation between state and behaviour.
  *
  * @author Alec Dorrington
  */
trait Rule:
  
  /** Returns the set of all successor states from the given state.
    * A successor state is any state that, according to this rule, could ''possibly'' be reached
    * from this state immediately after a ''single'' [[Input]] is taken by the user.
    *
    * Each successor state includes the most recent [[Input]] to have been selected.
    * This means that this method also implicitly enumerates legal [[Input]]s.
    *
    * Observe that the initial state is given as a [[HistoryState]],
    * while all successors are instead given as [[GameState]]s.
    * The difference is that a [[GameState]] also contains its own [[Rule]].
    * The significance of this here is that only ''this'' [[Rule]] is involved in generating successors,
    * not some other canonical [[Rule]] currently attached to the given state.
    * Whereas each successor is imbued with a new [[Rule]] which should apply thereafter.
    *
    * Successors are generated lazily as needed,
    * but repeated calls to this method will repeat the computation without caching.
    *
    * Game implementations typically shouldn't have to use this method directly.
    */
  final def next (state: HistoryState): LazyList[GameState] =
    successors(state).filter(_.turnId == state.turnId.next)
  
  /** Returns all [[GameState]]s that can immediately follow from the given one,
    * whether that be by following an [[Input]] or applying an [[Effect]].
    */
  private[dsl] def successors (state: HistoryState): LazyList[GameState]
  
  /** If this [[Rule]] can apply an immediate [[Effect]] to the given [[HistoryState]],
    * without first waiting for some [[Input]], then do so, otherwise return `None`.
    *
    * This is used in sequencing to apply [[Effect]]s immediately after each [[Input]].
    */
  private[dsl] def effect (state: HistoryState): Option[GameState]
  
  /** Imbue this [[Rule]] with a specific [[HistoryState]] to form a [[GameState]].
    * The resulting [[GameState]] represents the possibilities that arise from applying this [[Rule]] in the given [[HistoryState]].
    */
  final def from (state: HistoryState): GameState =
    ActiveState(state, this).applyEffect
  
  /** Imbue this [[Rule]] with the current (read: most up-to-date) [[HistoryState]] to form a [[GameState]].
    * The resulting [[GameState]] represents the possibilities that arise from applying this [[Rule]] in the current [[HistoryState]].
    */
  final def fromNow (using state: HistoryState): GameState =
    from(state)
  
  /** The union combinator is the standard way to provide multiple different options to the players.
    *
    * Specify that the player must do exactly one of two things, but that they may choose which.
    * The chosen subrule is then carried out in full, while the other option is discarded.
    *
    * This operator is both associative and commutative, and the identity under unioning is [[Cause.none]].
    *
    * Equivalent to [[Rule.union]].
    *
    * @example {{{
    *   val forward = Control.move(pawn, Dir.up.fromHere.ontoEmpty)
    *   val capture = Control.move(pawn, Dir.diagonallyUp.fromHere.ontoEnemy)
    *
    *   val rule = forward | capture
    * }}}
    */
  @targetName("union")
  final def | (that: => Rule): Rule = UnionRule(this, that)
  
  /** The sequence combinator.
    *
    * Specify that the player(s) and/or engine must do two separate things in sequence.
    * Both subrules must be completed, and in the specified order.
    *
    * Sequenced [[Effect]]s are applied immediately,
    * while sequenced [[Cause]]s wait for corresponding user [[Input]].
    *
    * Either operand may be an arbitrarily complex sequence of events.
    * The left path must be completed in full before the right may begin.
    * An exception exists where the left path ends in an [[Input]] that is optional,
    * in which case the right path may begin earlier.
    * In any case, taking an action from the right path will disallow all further choices on the left path.
    *
    * If the left path becomes blocked, so will the entire composite and the right path will never begin.
    *
    * This operator is associative but not commutative, and the identity under sequencing is [[Effect.identity]].
    *
    * Equivalent to [[Rule.sequence]].
    *
    * @example {{{
    *   val move = Control.moveThis(/* ... */)
    *   val promote = Rule.maybe(pawn.y == end)(pawn.promote(/* ... */))
    *
    *   val rule = move |> promote
    * }}}
    */
  @targetName("sequence")
  final def |> (that: => Rule): Rule =
    if this != Effect.identity
    then SequenceRule(this, that)
    else that
  
  /** Provide a fallback [[Rule]] to apply only when there is no legal
    * [[Input]] available under the default [[Rule]].
    *
    * The fallback will be triggered immediately upon blockage of the main path,
    * even for arbitrarily nested subrules.
    *
    * Once the fallback path is initiated, the default path will be permanently terminated.
    *
    * If the main path completes without becoming blocked, the fallback will never be triggered,
    * and control will be passed to the next [[Rule]] in the sequence as usual.
    *
    * On the left lies the default [[Rule]] to apply in the normal case.
    * On the right lies the fallback [[Rule]] to apply if the default is impossible.
    *
    * Equivalent to [[orElse]].
    */
  @targetName("orElse")
  final def ?: (that: Rule): Rule =
    AlternativeRule(that, this)
  
  /** Equivalent to [[?:]]. */
  def orElse (that: => Rule): Rule =
    AlternativeRule(this, that)
  
  /** Stop the game with the given [[Outcome]] if no legal [[Input]] is available.
    *
    * Equivalent to: {{{this.orElse(Effect.stop(outcome))}}}
    */
  def orElseStop(outcome: (state: HistoryState) ?=> Outcome): Rule =
    orElse(Effect.stop(outcome))
    
  /** Stop the game and declare it a [[Draw]] if no legal [[Input]] is available.
    *
    * Equivalent to: {{{this.orElseStop(Draw)}}}
    */
  def orElseDraw: Rule =
    orElseStop(Draw)
  
  /** Allow the player to skip this [[Rule]] if they so choose.
    *
    * In practice, this means the player may instead choose [[Input]]s from the next
    * [[Rule]] in the sequence in addition to [[Input]]s from this [[Rule]].
    *
    * Equivalent to: {{{this | Effect.identity}}}
    */
  final def optional: Rule =
    this | Effect.identity
  
  /** Repeat this [[Rule]] so long as the condition holds true.
    *
    * Equivalent to: {{{Rule.maybe(condition)(this |> this.repeatWhile(condition))}}}
    */
  final def repeatWhile (condition: HistoryState ?=> Boolean): Rule =
    Rule.maybe(condition)(this |> this.repeatWhile(condition))
  
  /** Repeat this [[Rule]] indefinitely, until the [[Game]] ends.
    *
    * Equivalent to: {{{this |> this |> this |> ...}}}
    */
  final def repeatForever: Rule =
    this |> this.repeatForever
  
  /** Repeat this [[Rule]] indefinitely, until the [[Game]] ends.
    * Following each repetition, the [[activePlayerId]] will be incremented by one (modulo the number of players).
    *
    * This is useful for implementing a standard game loop in which players take turns.
    *
    * Equivalent to: {{{(this |> Effect.endTurn).repeatForever}}}
    */
  final def alternatingTurns: Rule =
    (this |> Effect.endTurn).repeatForever
  
  /** Apply a filter to all successors produced by this [[Rule]].
    * Any [[Input]]s resulting in a state that doesn't satisfy the given predicate will be disallowed.
    *
    * Applies following each and every step throughout this [[Rule]].
    * At no point may the condition be violated.
    */
  final def require (condition: HistoryState ?=> Boolean): Rule =
    FilterRule(this, state => condition(using state))
  
object Rule:
  
  /** A context-dependent [[Rule]] which may select which specific
    * [[Rule]] applies, given the current [[HistoryState]].
    */
  def apply (brancher: (state: HistoryState) ?=> Rule): Rule =
    SwitchRule(state => brancher(using state))
  
  /** Apply the body [[Rule]] only if the condition is met.
    * Otherwise, this [[Rule]] may not be chosen.
    *
    * The condition is only checked when the [[Rule]] is initially triggered,
    * and isn't reaffirmed following each subsequent step therein.
    *
    * Recommended for use as part of a selection of [[Rule]]s composed under [[|]].
    * If composed with [[|>]], the path will be blocked if the condition fails.
    *
    * Defaults to [[Cause.none]] when the condition fails, and is thus equivalent to:
    * {{{Rule(if condition then rule else Cause.none)}}}
    *
    * @see [[Rule.maybe]] for a similar conditional [[Rule]]
    *      that will instead be skipped if the condition fails.
    */
  def when (condition: (state: HistoryState) ?=> Boolean) (rule: (state: HistoryState) ?=> Rule): Rule =
    Rule(if condition then rule else Cause.none)
  
  /** Apply the body [[Rule]] only if the condition is met.
    * Otherwise, this [[Rule]] may be freely skipped.
    *
    * The condition is only checked when the [[Rule]] is initially triggered,
    * and isn't reaffirmed following each subsequent step therein.
    *
    * Recommended for use as part of a sequence of [[Rule]]s composed under [[|>]].
    * If composed with [[|]], the player will be allowed to skip ''all'' alternatives if the condition fails.
    *
    * Defaults to [[Effect.identity]] when the condition fails, and is thus equivalent to:
    * {{{Rule(if condition then rule else Effect.identity)}}}
    *
    * @see [[Rule.when]] for a similar conditional [[Rule]]
    *      that will instead block the path if the condition fails.
    */
  def maybe (condition: (state: HistoryState) ?=> Boolean) (rule: (state: HistoryState) ?=> Rule): Rule =
    Rule(if condition then rule else Effect.identity)
  
  /** Equivalent to [[|]] with function notation and arbitrary arity,
    * i.e. {{{Rule.union(a, b, c, ...) == a | b | c | ...}}}
    */
  def union (rules: (state: HistoryState) ?=> Iterable[Rule]): Rule =
    Rule(rules.foldLeft[Rule](Cause.none)(_ | _))
  
  /** Equivalent to [[|>]] with function notation and arbitrary arity,
    * i.e. {{{Rule.sequence(a, b, c, ...) == a |> b |> c |> ...}}}
    */
  def sequence (rules: (state: HistoryState) ?=> Iterable[Rule]): Rule =
    Rule(rules.foldLeft[Rule](Effect.identity)(_ |> _))
  
  /** Allow the player to skip the given [[Rule]] if they so choose.
    *
    * In practice, this means the player may instead choose [[Input]]s from the next
    * [[Rule]] in the sequence in addition to [[Input]]s from rule.
    *
    * Equivalent to: {{{rule | Effect.identity}}}
    */
  def optional(rule: (state: HistoryState) ?=> Rule): Rule =
    Rule(rule).optional
    
  /** Repeat the given [[Rule]] so long as the condition holds true.
    *
    * Equivalent to: {{{Rule.maybe(condition)(rule |> rule.repeatWhile(condition))}}}
    */
  final def repeatWhile (condition: (state: HistoryState) ?=> Boolean) (rule: (state: HistoryState) ?=> Rule): Rule =
    Rule(rule).repeatWhile(condition)
  
  /** Repeat the given [[Rule]] indefinitely, until the [[Game]] ends.
    *
    * Equivalent to: {{{rule |> rule |> rule |> ...}}}
    */
  def repeatForever (rule: (state: HistoryState) ?=> Rule): Rule =
    Rule(rule).repeatForever
  
  /** Repeat the given [[Rule]] indefinitely, until the [[Game]] ends.
    * Following each repetition, the [[activePlayerId]] will be incremented by one (modulo the number of players).
    *
    * This is useful for implementing a standard game loop in which players take turns.
    *
    * Equivalent to: {{{(rule |> Effect.endTurn).repeatForever}}}
    */
  def alternatingTurns (rule: (state: HistoryState, player: PlayerRef) ?=> Rule): Rule =
    Rule:
      given PlayerRef = summon[HistoryState].activePlayer
      rule
    .alternatingTurns
  
  /** A [[Rule]] for allowing the user to choose one of two options.
    * Implements the [[|]] operator.
    * @param left The first possible option.
    * @param right The second possible option.
    */
  private[rules] class UnionRule (
    left: => Rule,
    right: => Rule,
  ) extends Rule:
    
    override def successors(state: HistoryState) =
      left.successors(state) ++ right.successors(state)
    
    // As soon as there are multiple choices, the player must choose one, so no Effect may be applied.
    def effect(state: HistoryState) = None
  
  /** A [[Rule]] for applying multiple subrules in sequence.
    * Implements the [[|>]] operator.
    * @param left The first [[Rule]] in the sequence.
    * @param right The second [[Rule]] in the sequence.
    */
  private[rules] class SequenceRule (
    left: => Rule,
    right: => Rule,
  ) extends Rule:
    
    def successors(s0: HistoryState) =
      left.successors(s0)
        // Each successor still needs to remember to apply the RHS when it is done.
        .map(_.updateRule(_ |> right))
        .flatMap: s1 =>
          // Search to a depth of exactly one Input, but apply any number of Effects.
          if s0.turnId == s1.turnId then s1.next
          else if s0.turnId.next == s1.turnId then Some(s1.applyEffect)
          else throw new IllegalStateException
      
    def effect(s0: HistoryState) =
      left.effect(s0)
        .map(_.updateRule(_ |> right))
        .map(_.applyEffect)
  
  /** A [[Rule]] which decides which other [[Rule]] to apply based on the current state.
    * @param brancher The function for selecting a Rule.
    */
  private[rules] class SwitchRule (
    brancher: HistoryState => Rule,
  ) extends Rule:
    
    private val memo: mutable.Map[HistoryState, Rule] = mutable.Map.empty
    private def branch(state: HistoryState): Rule =
      memo.getOrElseUpdate(state, brancher(state))
    
    def successors(state: HistoryState) =
      branch(state).successors(state)
    
    def effect(state: HistoryState) =
      branch(state).effect(state)
  
  /** A [[Rule]] which filters successors based on a condition.
    * Implements the [[require]] method.
    * @param base The base Rule to which the filter is applied.
    * @param condition The condition that must be satisfied by all successors.
    */
  private[rules] class FilterRule (
    base: Rule,
    condition: HistoryState => Boolean,
  ) extends Rule:
    
    def successors(state: HistoryState) =
      base.successors(state)
        // Filter the successors based on the condition.
        .filter(state => condition(state.history))
        // Each successor still needs to remember to apply the filter to future Inputs.
        .map(_.updateRule(FilterRule(_, condition)))
    
    def effect(state: HistoryState) =
      base.effect(state)
  
  /**
    * A [[Rule]] which applies the default Rule if possible, otherwise the fallback Rule.
    * Implements the [[?:]] operator.
    * @param default The default Rule which applies in the normal case.
    * @param fallback The fallback Rule which applies if the default is impossible.
    */
  private[rules] class AlternativeRule (
    default: => Rule,
    fallback: => Rule,
  ) extends Rule:
    
    def successors(state: HistoryState) =
      if default.from(state).isBlocked
      then fallback.successors(state)
      else default.successors(state)
        .map(_.updateRule(_.orElse(fallback)))
      
    def effect(state: HistoryState) =
      if default.from(state).isBlocked
      then fallback.effect(state)
      else default.effect(state)