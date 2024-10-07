package boards.algebra

import boards.imports.games.{*, given}

trait Game:
  
  protected type GameBoard = Kernel[Colour]
  protected val Board: GameBoard
  
  def setup(numPlayers: Int): InstantaneousState ?=> InstantaneousState
  
  def rules: Rule
  def initial(numPlayers: Int): InitialState =
    val genesis = InstantaneousState.initial(Board, numPlayers)
    GameState.initial(this, setup(numPlayers)(using genesis), rules)
    
  val data: Game.Metadata = Game.Metadata (
    name = getClass.getSimpleName,
    numPlayers = Seq(2),
    playerNames = Seq("Player 1", "Player 2"),
  )
  export data.*
  
object Game:
  
  def none: Game = new Game.WithMetadata:
    def setup(numPlayers: Int) = InstantaneousState.empty
    def rules = Rule.none
    val Board = Kernel.empty
  
  case class Metadata (
    name: String,
    numPlayers: Seq[Int],
    playerNames: Seq[String],
  )
  
  abstract class WithMetadata (
    name: String = "",
    numPlayers: Seq[Int] = Seq.empty,
    playerNames: Seq[String] = Seq.empty,
  ) extends Game:
    
    private val _name = if name.isEmpty then getClass.getSimpleName else name
    private val _numPlayers = if numPlayers.isEmpty then Seq(2) else numPlayers
    private val _playerNames = if playerNames.isEmpty then (1 to _numPlayers.max).map(i => s"Player $i") else playerNames
    
    override val data = Metadata (
      _name,
      _numPlayers,
      _playerNames,
    )