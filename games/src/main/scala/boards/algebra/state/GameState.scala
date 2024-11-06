package boards.algebra.state

import boards.algebra.Action.Skip
import boards.algebra.rules.{Capability, Rule}
import boards.imports.games.{*, given}
import boards.imports.math.{*, given}

import scala.annotation.{tailrec, targetName}

sealed trait GameState:
  
  private val capability = Capability(ruleOption.getOrElse(Rule.none), this)
  export capability.*
  
  val now: InstantaneousState
  
  def actionOption: Option[Action]
  def withAction(action: Action): NonInitialState
  
  def previousOption: Option[NonFinalState]
  def outcomeOption: Option[Outcome]
  
  def withBoard(board: InstantaneousState): GameState
  
  def ruleOption: Option[Rule]
  def withRule(rule: => Rule): GameState
  def updateRule(f: Rule => Rule): GameState
  
  def flattenPastSkips: GameState
  def flattenFutureSkips: GameState
  
  def takeAction(action: Action): Option[GameState] =
    next.find(_.actionOption.contains(action))
    
  def takeActionWhere(f: Action => Boolean): Option[GameState] =
    for
      action <- actions.find(f)
      result <- takeAction(action)
    yield result
    
  def place(pos: VecI): Option[GameState] =
    takeActionWhere:
      case Place(_, _, `pos`) => true
      case _ => false
    
  def move(from: VecI, to: VecI): Option[GameState] =
    takeActionWhere:
      case Move(_, `from`, `to`) => true
      case _ => false
      
  def destroy(pos: VecI): Option[GameState] =
    takeActionWhere:
      case Destroy(piece) => piece.position == pos
      case _ => false
      
  def atTime(t: Int): Option[GameState] =
    if t < time then previousOption.flatMap(_.atTime(t))
    else if t == time then Some(this)
    else None
  
  /** A version of this state whereby no further actions are possible. */
  def inert: GameState
  
  def actionHashes: Iterator[String] = actions.map(_.hash)
  def findActionByHash(hash: String): Option[Action] =
    actions.find(_.hash == hash)
  def takeActionByHash(hash: String): Option[GameState] =
    next.find(_.actionOption.exists(_.hash == hash))
    
  def isInitial: Boolean = this match
    case _: InitialState => true
    case _ => false
    
  def isFinal: Boolean = this match
    case _: FinalState => true
    case _ => false
    
  def turnStart: GameState =
    previousOption match
      case Some(prev) if prev.now.activePlayer == now.activePlayer => prev.turnStart
      case _ => this
      
  def actionDiff: Iterator[VecI] =
    previousOption match
      case Some(prev) => now.diff(prev.now)
      case None => Iterator.empty
        
  def turnDiff: Iterator[VecI] =
    previousOption match
      case Some(prev) => now.diff(prev.turnStart.now)
      case None => Iterator.empty
  
  val time: Int
  val game: Game
  
  export now.activePlayer
  
object GameState:
  
  sealed trait NonFinalState extends GameState:
    def rule: Rule
    def outcomeOption: None.type = None
    def ruleOption: Some[Rule] = Some(rule)
    override def withBoard(board: InstantaneousState): NonFinalState
    def withRule(rule: => Rule): NonFinalState
    override def updateRule(f: Rule => Rule): NonFinalState
    override def withAction(action: Action): InterimState
    /*def successor(action: Action, state: InstantaneousState = now): InterimState =
      InterimState(state, action, predecessor, Rule.skip)
    @tailrec
    private def predecessor: NonFinalState = this match
      case InterimState(_, Action.Skip, prev, _) => prev.predecessor
      case s => s*/
  
  sealed trait NonInitialState extends GameState:
    val action: Action
    val previous: NonFinalState
    def actionOption: Some[Action] = Some(action)
    def previousOption: Some[NonFinalState] = Some(previous)
    val time: Int = previous.time + 1
    val game: Game = previous.game
    def withBoard(board: InstantaneousState): NonInitialState
    def withRule(rule: => Rule): NonInitialState
    def updateRule(f: Rule => Rule): NonInitialState
    def withOutcome(outcome: Outcome): FinalState =
      FinalState(now, action, previous, outcome)
    def withPrevious(state: NonFinalState): NonInitialState
  
  case class InitialState (
    game: Game,
    now: InstantaneousState,
    rule: Rule,
  ) extends NonFinalState:
    def actionOption: None.type = None
    def previousOption: None.type = None
    def inert: InitialState = copy(rule = Rule.none)
    val time: 0 = 0
    def withBoard(board: InstantaneousState): InitialState = copy(now = board)
    def withRule(rule: => Rule): InitialState = copy(rule = rule)
    def updateRule(f: Rule => Rule): InitialState = copy(rule = f(rule))
    def withAction(action: Action): InterimState = InterimState(now, action, this, rule)
    def flattenPastSkips: InitialState = this
    def flattenFutureSkips: GameState =
      if actions.nonEmpty && actions.forall(_ == Skip) && rule != Rule.skip
      then next.take(1).toSeq.head.flattenPastSkips.flattenFutureSkips
      else flattenPastSkips
  
  case class InterimState (
    now: InstantaneousState,
    action: Action,
    previous: NonFinalState,
    rule: Rule
  ) extends NonInitialState, NonFinalState:
    def inert: InterimState = copy(rule = Rule.none)
    def withBoard(board: InstantaneousState): InterimState = copy(now = board)
    def withRule(rule: => Rule): InterimState = copy(rule = rule)
    def updateRule(f: Rule => Rule): InterimState = copy(rule = f(rule))
    def withAction(action: Action): InterimState = copy(action = action)
    def withPrevious(state: NonFinalState): InterimState = copy(previous = state)
    @tailrec
    final def flattenPastSkips: NonFinalState = (action, previous) match
      case (Action.Skip, previous: InterimState) =>
        InterimState(now, previous.action, previous.previous, rule).flattenPastSkips
      case (Action.Skip, previous: InitialState) =>
        InitialState(previous.game, now, rule)
      case _ => this
    def flattenFutureSkips: GameState =
      if actions.nonEmpty && actions.forall(_ == Skip) && rule != Rule.skip
      then next.take(1).toSeq.head.flattenPastSkips.flattenFutureSkips
      else flattenPastSkips
  
  case class FinalState (
    now: InstantaneousState,
    action: Action,
    previous: NonFinalState,
    outcome: Outcome
  ) extends NonInitialState:
    def inert: FinalState = this
    def outcomeOption: Some[Outcome] = Some(outcome)
    def ruleOption: None.type = None
    def withBoard(board: InstantaneousState): FinalState = copy(now = board)
    def withRule(rule: => Rule): FinalState = this
    def updateRule(f: Rule => Rule): FinalState = this
    def withAction(action: Action): FinalState = copy(action = action)
    def withPrevious(state: NonFinalState): FinalState = copy(previous = state)
    @tailrec
    final def flattenPastSkips: FinalState = (action, previous) match
      case (Skip, previous: NonInitialState) =>
        FinalState(now, previous.action, previous.previous, outcome).flattenPastSkips
      case _ => this
    def flattenFutureSkips: FinalState = this
  
  sealed trait Outcome
  case class Winner(player: Game.PlayerId) extends Outcome
  case object Draw extends Outcome
  
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