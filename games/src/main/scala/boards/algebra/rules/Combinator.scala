package boards.algebra.rules

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.algebra.rules.Rule.Query

import scala.collection.mutable

/**
 * A rule representing a composition of other rules.
 */
trait Combinator extends Rule

object Combinator:
  
  /**
   * A rule which changes based on the current game state.
   *
   * @param brancher the function for determining the current rule.
   */
  private[algebra] class SwitchRule (
    brancher: GameState => Rule,
  ) extends Combinator:
    
    val memo: mutable.Map[GameState, Rule] = mutable.Map.empty
    private def branch(state: GameState): Rule =
      memo.getOrElseUpdate(state, brancher(state))
    
    def next(state: GameState, query: Query): Iterator[GameState] =
      branch(state).next(state, query)
    
    // Optimisation: Not required for correctness!
    override def actions(state: GameState, query: Query) =
      branch(state).actions(state, query)
  
  /**
   * A rule representing the union composition of two other rules.
   * Produced by use of the '|' operator.
   *
   * @param left this first option.
   * @param right the second option.
   */
  private[algebra] class UnionRule (
    left: => Rule,
    right: => Rule,
  ) extends Combinator:
    
    def next(state: GameState, query: Query): Iterator[GameState] =
      (left.next(state, query) ++ right.next(state, query))
        .distinctBy(_.actionOption)
    
    // Optimisation: Not required for correctness!
    override def actions(state: GameState, query: Query): Iterator[Action] =
      (left.actions(state, query) ++ right.actions(state, query))
        .distinct
  
  /**
   * A rule representing the sequential composition of two other rules.
   * Produced by use of the '|>' operator.
   *
   * @param left the first rule to complete.
   * @param right the second rule to complete.
   */
  private[algebra] class SequenceRule (
    left: => Rule,
    right: => Rule,
  ) extends Combinator:
    
    def next(state: GameState, query: Query): Iterator[GameState] =
      left.next(state, query)
        .map(_.updateRule(_ |> right))
        //.map(_.flattenFutureSkips)
  
  /**
   * A rule for conditionally disallowing some successor states.
   *
   * @param base the original rule.
   * @param condition the additional condition that all successors must satisfy.
   */
  private[algebra] class FilterRule (
    base: Rule,
    condition: GameState => Boolean,
  ) extends Combinator:
    
    def next(state: GameState, query: Query): Iterator[GameState] =
      base.next(state, query)
        .filter(state => state.actionOption.contains(Action.skip) || condition(state))
        .map: state =>
          if state.actionOption.contains(Action.skip) then state
          else state.updateRule(_.require(condition(summon[GameState])))