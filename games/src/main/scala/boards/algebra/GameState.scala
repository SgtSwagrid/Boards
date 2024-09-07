package boards.algebra

import boards.GameImports.{*, given}
import boards.algebra.PieceSet.Diff
import scala.annotation.targetName

sealed trait GameState:
  def now: InstantaneousState
  def actionOption: Option[Action]
  def previousOption: Option[NonFinalState]
  
  def withBoard(board: InstantaneousState): GameState =
    lazy val newState: GameState = this match
      case InitialState(_, r) => InitialState(board, r)
      case InterimState(_, a, p, r) => InterimState(board, a, p, r)
      case FinalState(_, a, p, o) => FinalState(board, a, p, o)
    newState
  
  def next: Iterator[NonInitialState]
  def actions: Iterator[Action]
  def takeAction(action: Action): Option[NonInitialState] =
    next.find(_.action == action)
  
  def actionHashes: Iterator[Int] = actions.map(_.hashCode)
  def findActionByHash(hash: Int): Option[Action] =
    actions.find(_.hashCode == hash)
  def takeActionByHash(hash: Int): Option[NonInitialState] =
    next.find(_.action.hashCode == hash)
  
object GameState:
  
  sealed trait NonFinalState extends GameState:
    def rule: Rule
    def next: Iterator[NonInitialState] = rule.next(this)
    def actions: Iterator[Action] = rule.actions(this)
  
  sealed trait NonInitialState extends GameState:
    val action: Action
    val previous: NonFinalState
    def actionOption: Some[Action] = Some(action)
    def previousOption: Some[NonFinalState] = Some(previous)
    def diff: Seq[Diff] = PieceSet.diff(previous.now.pieces, now.pieces)
  
  case class InitialState (
    now: InstantaneousState,
    rule: Rule
  ) extends NonFinalState:
    def actionOption: None.type = None
    def previousOption: None.type = None
  
  case class InterimState (
    now: InstantaneousState,
    action: Action,
    previous: NonFinalState,
    rule: Rule
  ) extends NonInitialState, NonFinalState
  
  case class FinalState (
    now: InstantaneousState,
    action: Action,
    previous: NonFinalState,
    outcome: Outcome
  ) extends NonInitialState:
    def next: Iterator[Nothing] = Iterator.empty
    def actions: Iterator[Nothing] = Iterator.empty
  
  enum Outcome:
    case Winner(player: Int)
    case Draw
  
  def initial (
    board: InstantaneousState,
    rule: Rule
  ): InitialState =
    InitialState(board, rule)
    
  def empty: InitialState =
    InitialState(InstantaneousState.empty, Rule.none)
  
  /*@targetName("cause") object ~> :
    def unapply (
      state: GameState
    ): Option[(Action, BoardState)] =
      state match
        case InitialState(_, _) => None
        case InterimState(board, action, _, _) => Some((action, board))
        case FinalState(board, action, _, _) => Some((action, board))*/
  
  /*@targetName("successor") object |> :
    def unapply (
      state: GameState
    ): Option[(NonFinalState, NonInitialState)] =
      state match
        case InitialState(_, _) => None
        case state @ InterimState(_, _, previous, _) => Some((previous, state))
        case state @ FinalState(_, _, previous, _) => Some((previous, state))*/
        
  object Following:
    def unapply (
      state: GameState
    ): Option[Action] =
      state.actionOption