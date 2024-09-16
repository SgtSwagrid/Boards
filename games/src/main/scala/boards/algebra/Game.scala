package boards.algebra

import boards.imports.games.{*, given}

import boards.algebra.Game.ID

trait Game:
  
  protected type GameBoard = Kernel[Colour]
  protected val Board: GameBoard
  
  def setup(numPlayers: Int): InstantaneousState ?=> InstantaneousState
  
  def rules: Rule
  def initial(numPlayers: Int): InitialState =
    val genesis = InstantaneousState.initial(Board, numPlayers)
    GameState.initial(setup(numPlayers)(using genesis), rules)
  
  val id = { ID += 1; ID - 1 }
  
  def numPlayers: Seq[Int] = Seq(2)
  
object Game:
  
  private var ID = 0