package boards.algebra

import boards.GameImports.{*, given}
import scala.annotation.{tailrec, targetName}

trait Rule:
  import Rule.*
  
  def next(state: NonFinalState): Iterator[NonInitialState]
  def actions(state: NonFinalState): Iterator[Action]
  def updateGenerators(filter: PartialFunction[Generator, Rule]): Rule
  
  def after(f: Stateful[BoardState]): Rule =
    TransformedRule(this, s => f(using s).lift(s).getOrElse(s))
    
  def require(f: Stateful[Boolean]): Rule =
    FilteredRule(this, s => f(using s).lift(s).getOrElse(false))
  
  @targetName("union")
  def | (that: Stateful[Rule]): Rule =
    UnionRule(this, Rule.when(that))
  
  @targetName("andThen")
  def |> (that: => Stateful[Rule]): Rule =
    SequenceRule(this, Rule.sometimes(that))
  
  def optional: Rule = this | Rule.skip
  
  def repeatWhile(f: Stateful[Boolean]): Rule =
    Rule.sometimes:
      case state if f.lift(state).getOrElse(false) =>
        this |> this.repeatWhile(f)
      
  def repeatForever: Rule =
    this |> this.repeatForever
  
  def repeat(n: Int): Rule =
    n match
      case 0 => Rule.skip
      case n => this |> this.repeat(n - 1)
      
  def orElse(f: Stateful[Rule]): Rule =
    Rule.when:
      case state if from(state).canAct => this
      case state => f.lift(state).getOrElse(Rule.none)
      
  def stop(f: Stateful[Outcome]): Rule =
    HaltingRule(this, s => f(using s).lift(s))
    
  def stopIf
    (cond: Stateful[Boolean])
    (outcome: => Stateful[Outcome])
  : Rule =
    HaltingRule(this, s =>
      for
        stop <- cond(using s).lift(s) if stop
        result <- outcome(using s).lift(s)
      yield result
    )
    
  def stopIfImpossible(f: => Stateful[Outcome]): Rule = orElse(stop(f))
  
  def from(state: GameState): Capability = Capability(this, state)
  
object Rule:
  
  type Stateful[X] = GameState ?=> PartialFunction[GameState, X]
  
  def none: Rule = Generator.none
  def skip: Rule = Generator.skip
  
  def onCondition
    (f: Stateful[Boolean])
    (rule: Stateful[Rule])
  : Rule =
    Rule.when:
      case state if rule.isDefinedAt(state) =>
        rule(state)
  
  def when(f: Stateful[Rule]): Rule =
    SwitchedRule(s => f(using s).lift(s).getOrElse(Rule.none))
  
  def sometimes(f: Stateful[Rule]): Rule =
    SwitchedRule(s => f(using s).lift(s).getOrElse(Rule.skip))
    
  def apply(f: GameState ?=> Rule): Rule =
    SwitchedRule(s => f(using s))
    
  def repeatWhile
    (f: Stateful[Boolean])
    (rule: Stateful[Rule])
  : Rule =
    Rule.when:
      case state if rule.isDefinedAt(state) =>
        rule(state).repeatWhile(f)
        
  def alternatingTurns(rule: Stateful[Rule]): Rule =
    Rule.when(rule).after(_.endTurn).repeatForever
  
  private class TransformedRule (
    base: Rule,
    f: GameState => BoardState
  ) extends Rule:
    
    def next(state: NonFinalState) =
      base.next(state).map:
        case s @ FinalState(_, a, p, o) => FinalState(f(s), a, p, o)
        case s @ InterimState(_, a, p, r)
          if r.from(s).isOptional =>
            InterimState(f(s), a, p, Rule.skip)
        case s @ InterimState(_, a, p, r) => InterimState(s, a, p, r.after(f(_)))
    
    export base.actions
    
    def updateGenerators(filter: PartialFunction[Generator, Rule]) =
      TransformedRule(base.updateGenerators(filter), f)
  
  private class HaltingRule (
    base: Rule,
    f: GameState => Option[Outcome]
  ) extends Rule:
    
    def next(state: NonFinalState) =
      base.next(state).map:
        case s: FinalState => s
        case s @ InterimState(_, a, p, r)
          if r.from(s).isOptional && f(s).isDefined =>
            FinalState(s, a, p, f(s).get)
        case InterimState(s, a, p, r) =>
          InterimState(s, a, p, HaltingRule(r, f))
    
    export base.{actions, updateGenerators}
  
  private class FilteredRule (
    base: Rule,
    f: NonInitialState => Boolean
  ) extends Rule:
    
    def next(state: NonFinalState) =
      base.next(state).filter(f)
    
    def actions(state: NonFinalState) =
      next(state).map(_.action)
      
    def updateGenerators(filter: PartialFunction[Generator, Rule]) =
      FilteredRule(base.updateGenerators(filter), f)

  private class UnionRule (
    left: Rule,
    right: Rule
  ) extends Rule:
    
    def next(state: NonFinalState) =
      left.next(state) ++ right.next(state)
    
    def actions(state: NonFinalState) =
      left.actions(state) ++ right.actions(state)
      
    def updateGenerators(filter: PartialFunction[Generator, Rule]) =
      UnionRule(left.updateGenerators(filter), right.updateGenerators(filter))
  
  private class SwitchedRule (
    f: NonFinalState => Rule
  ) extends Rule:
    
    def next(state: NonFinalState) =
      f(state).next(state)
      
    def actions(state: NonFinalState) =
      f(state).actions(state)
      
    def updateGenerators(filter: PartialFunction[Generator, Rule]) =
      SwitchedRule(f.andThen(_.updateGenerators(filter)))
  
  private class SequenceRule (
    left: Rule,
    right: => Rule
  ) extends Rule:
    
    def next(state: NonFinalState) =
      left.next(state).flatMap:
        case s: FinalState => Some(s)
        case InterimState(_, NoOp, _, _) => right.next(state)
        case InterimState(s, a, p, r) => Some(InterimState(s, a, p, r |> right))
    
    def actions(state: NonFinalState) =
      left.actions(state).flatMap:
        case NoOp => right.actions(state)
        case a => Some(a)
      
    def updateGenerators(filter: PartialFunction[Generator, Rule]) =
      SequenceRule(left.updateGenerators(filter), right.updateGenerators(filter))
      
  private class AccumulatedRule[X] (
    base: Rule,
    value: X,
    f: (X, GameState) => X
  ) extends Rule:
    
    def next(state: NonFinalState) =
      base.next(state).map:
        case s: FinalState => s
        case s @ InterimState(_, a, p, r) =>
          InterimState(s, a, p, AccumulatedRule(r, f(value, s), f))
          
    export base.{actions, updateGenerators}
    
  given Conversion[Iterable[Rule], Rule] with
    def apply(rules: Iterable[Rule]): Rule = rules.foldLeft(Rule.none)(_ | _)
  
  given [X <: Rule | BoardState | Boolean | Outcome | PieceSet]: Conversion[X, PartialFunction[GameState, X]] with
    def apply(x: X): PartialFunction[GameState, X] = _ => x
    
  given Conversion[PieceSet, PartialFunction[GameState, BoardState]] with
    def apply(pieces: PieceSet): PartialFunction[GameState, BoardState] = pieces