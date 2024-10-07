package boards.algebra

import boards.imports.games.{*, given}
import scala.annotation.targetName

sealed trait GameState:
  val now: InstantaneousState
  def actionOption: Option[Action]
  def previousOption: Option[NonFinalState]
  
  def withBoard(board: InstantaneousState): GameState =
    lazy val newState: GameState = this match
      case InitialState(g, _, r) => InitialState(g, board, r)
      case InterimState(_, a, p, r) => InterimState(board, a, p, r)
      case FinalState(_, a, p, o) => FinalState(board, a, p, o)
    newState
  
  def next: Iterator[NonInitialState]
  def actions: Iterator[Action]
  def takeAction(action: Action): Option[NonInitialState] =
    next.find(_.action == action)
    
  def takeActionWhere(f: Action => Boolean): Option[NonInitialState] =
    for
      action <- actions.find(f)
      result <- takeAction(action)
    yield result
    
  def place(pos: VecI): Option[NonInitialState] =
    takeActionWhere:
      case p @ Place(_, _, `pos`) => true
      case _ => false
    
  def move(from: VecI, to: VecI): Option[NonInitialState] =
    takeActionWhere:
      case m @ Move(_, `from`, `to`) => true
      case _ => false
      
  def destroy(pos: VecI): Option[NonInitialState] =
    takeActionWhere:
      case d @ Destroy(piece) => piece.position == pos
      case _ => false
      
  def inert: GameState
  
  def actionHashes: Iterator[Int] = actions.map(_.hashCode)
  def findActionByHash(hash: Int): Option[Action] =
    actions.find(_.hashCode == hash)
  def takeActionByHash(hash: Int): Option[NonInitialState] =
    next.find(_.action.hashCode == hash)
    
  val time: Int
  val game: Game
  
  export now.activePlayer
  
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
    val time: Int = previous.time + 1
    val game: Game = previous.game
  
  case class InitialState (
    game: Game,
    now: InstantaneousState,
    rule: Rule,
  ) extends NonFinalState:
    def actionOption: None.type = None
    def previousOption: None.type = None
    def inert: InitialState = copy(rule = Rule.none)
    val time: 0 = 0
  
  case class InterimState (
    now: InstantaneousState,
    action: Action,
    previous: NonFinalState,
    rule: Rule
  ) extends NonInitialState, NonFinalState:
    def inert: InterimState = copy(rule = Rule.none)
  
  case class FinalState (
    now: InstantaneousState,
    action: Action,
    previous: NonFinalState,
    outcome: Outcome
  ) extends NonInitialState:
    def next: Iterator[Nothing] = Iterator.empty
    def actions: Iterator[Nothing] = Iterator.empty
    def inert: FinalState = this
  
  enum Outcome:
    case Winner(player: Int)
    case Draw
  
  def initial (
    game: Game,
    board: InstantaneousState,
    rule: Rule
  ): InitialState =
    InitialState(game, board, rule)
    
  def empty: InitialState =
    InitialState(Game.none, InstantaneousState.empty, Rule.none)
  
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