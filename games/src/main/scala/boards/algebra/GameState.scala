package boards.algebra

import boards.GameImports.{*, given}
import scala.annotation.targetName

sealed trait GameState:
  def boardState: BoardState
  def actionOption: Option[Action]
  def previousOption: Option[NonFinalState]
  
  def withBoard(board: BoardState): GameState =
    lazy val newState: GameState = this match
      case InitialState(_, r) => InitialState(board, r)
      case InterimState(_, a, p, r) => InterimState(board, a, p, r)
      case FinalState(_, a, p, o) => FinalState(board, a, p, o)
    newState
  
object GameState:
  
  sealed trait NonFinalState extends GameState:
    def rule: Rule
    def next: Iterator[NonInitialState] = rule.next(this)
    def actions: Iterator[Action] = rule.actions(this)
    //def isLegal(action: Action): Boolean = rule.isLegal(this, action)
    def takeAction(action: Action): Option[NonInitialState] =
      next.find(_.action == action)
  
  sealed trait NonInitialState extends GameState:
    val action: Action
    val previous: NonFinalState
    def actionOption: Some[Action] = Some(action)
    def previousOption: Some[NonFinalState] = Some(previous)
  
  case class InitialState (
    boardState: BoardState,
    rule: Rule
  ) extends NonFinalState:
    def actionOption: None.type = None
    def previousOption: None.type = None
  
  case class InterimState (
    boardState: BoardState,
    action: Action,
    previous: NonFinalState,
    rule: Rule
  ) extends NonInitialState with NonFinalState
  
  case class FinalState (
    boardState: BoardState,
    action: Action,
    previous: NonFinalState,
    outcome: Outcome
  ) extends NonInitialState
  
  enum Outcome:
    case Winner(player: Int)
    case Draw
  
  def initial (
    board: BoardState,
    rule: Rule
  ): InitialState =
    InitialState(board, rule)
  
  @targetName("cause") object ~> :
    def unapply (
      state: GameState
    ): Option[(Action, BoardState)] =
      state match
        case InitialState(_, _) => None
        case InterimState(board, action, _, _) => Some((action, board))
        case FinalState(board, action, _, _) => Some((action, board))
  
  @targetName("successor") object |> :
    def unapply (
      state: GameState
    ): Option[(NonFinalState, NonInitialState)] =
      state match
        case InitialState(_, _) => None
        case state @ InterimState(_, _, previous, _) => Some((previous, state))
        case state @ FinalState(_, _, previous, _) => Some((previous, state))
        
  object Following:
    def unapply (
      state: GameState
    ): Option[Action] =
      state.actionOption