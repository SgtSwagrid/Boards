package boards.dsl.rules

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import Rule.*
import Cause.UnionCause
import Effect.SequenceEffect
import io.circe.Decoder.state

import scala.annotation.targetName
import scala.collection.mutable

trait Rule:
  
  def successors(state: HistoryState): LazyList[GameState]
  def effect(state: HistoryState): Option[GameState]
  
  final def from(state: HistoryState): GameState = ActiveState(state, this)
  final def fromNow(using state: HistoryState): GameState = ActiveState(state, this)
  
  @targetName("union")
  final def | (that: => Rule): Rule = UnionRule(this, that)
  
  @targetName("sequence")
  final def |> (that: => Rule): Rule =
    if this != Effect.identity
    then SequenceRule(this, that)
    else that
  
  @targetName("orElse")
  final def ?: (that: Rule): Rule = Rule:
     Alternative(that, this)
  
  final def optional: Rule =
    this | Effect.identity
  
  final def repeatForever: Rule =
    this |> this.repeatForever
  
  final def alternatingTurns: Rule =
    (this |> Effect.endTurn).repeatForever
  
  final def require(condition: HistoryState ?=> Boolean): Rule =
    FilterRule(this, state => condition(using state))
    
  def structure(state: HistoryState): String
  
object Rule:
  
  /*type Union[L <: Rule, R <: Rule] <: Rule = (L, R) match
    case (Cause, Cause) => Cause
    case (Rule, Rule) => Rule
  
  type Sequence[L <: Rule, R <: Rule] <: Rule = (L, R) match
    case (Effect, Effect) => Effect
    case (Rule, Rule) => Rule
  
  extension [L <: Rule] (left: L)
    
    def | [R <: Rule] (right: => R): Union[L, R] = (left, right) match
      case rules: (Cause, Cause) => UnionCause(rules(0), rules(1))
      case rules: (Rule, Rule) => UnionRule(rules(0), rules(1))
      
    def |> [R <: Rule] (right: => R): Sequence[L, R] = (left, right) match
      case rules: (Effect, Effect) => SequenceEffect(rules(0), rules(1))
      case rules: (Rule, Rule) => SequenceRule(rules(0), rules(1))*/
  
  def apply(brancher: HistoryState ?=> Rule): Rule =
    SwitchRule(state => brancher(using state))
  
  def when(condition: HistoryState ?=> Boolean)(rule: HistoryState ?=> Rule): Rule =
    Rule(if condition then rule else Cause.none)
    
  def maybe(condition: HistoryState ?=> Boolean)(rule: HistoryState ?=> Rule): Rule =
    Rule(if condition then rule else Effect.identity)
  
  def union(rules: HistoryState ?=> Iterable[Rule]): Rule =
    Rule(rules.foldLeft[Rule](Cause.none)(_ | _))
    
  def sequence(rules: HistoryState ?=> Iterable[Rule]): Rule =
    Rule(rules.foldLeft[Rule](Effect.identity)(_ |> _))
    
  def optional(rule: HistoryState ?=> Rule): Rule =
    Rule(rule).optional
    
  def repeatForever(rule: HistoryState ?=> Rule): Rule =
    Rule(rule).repeatForever
    
  def alternatingTurns(rule: HistoryState ?=> Rule): Rule =
    Rule(rule).alternatingTurns
  
  private[rules] class UnionRule (
    left: => Rule,
    right: => Rule,
  ) extends Rule:
    
    override def successors(state: HistoryState) =
      left.successors(state) ++ right.successors(state)
    
    def effect(state: HistoryState) = None
    
    def structure(state: HistoryState) =
      s"(${left.structure(state)} | ${right.structure(state)})"
  
  private[rules] class SequenceRule (
    left: => Rule,
    right: => Rule,
  ) extends Rule:
    
    def successors(s0: HistoryState) =
      left.successors(s0)
        .map(_.updateRule(_ |> right))
        .flatMap: s1 =>
          if s0.turnId == s1.turnId then s1.successors
          else if s0.turnId.next == s1.turnId then Some(s1.applyEffect)
          else throw new IllegalStateException
      
    def effect(s0: HistoryState) =
      left.effect(s0)
        .map(_.updateRule(_ |> right))
        .map(_.applyEffect)
      
    def structure(state: HistoryState) =
      s"(${left.structure(state)} |> ${right.structure(state)})"
    
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
      
    def structure(state: HistoryState) = branch(state).structure(state)
      
  private[rules] class FilterRule (
    base: Rule,
    condition: HistoryState => Boolean,
  ) extends Rule:
    
    def successors(state: HistoryState) =
      base.successors(state)
        .filter(state => condition(state.history))
        .map(_.updateRule(_.require(condition(summon[HistoryState]))))
    
    def effect(state: HistoryState) =
      base.effect(state)
    
    def structure(state: HistoryState) = s"${base.structure(state)}?"
    
  private[rules] class Alternative (
    base: Rule,
    alternative: Rule,
  ) extends Rule:
    
    def successors(state: HistoryState) =
      val states = base.successors(state)
        .map(_.updateRule(_ ?: alternative))
      if states.nonEmpty then states else alternative.successors(state)
      
    def effect(state: HistoryState) =
      base.effect(state)
      
    def structure(state: HistoryState) = s"($base ?: $alternative)"