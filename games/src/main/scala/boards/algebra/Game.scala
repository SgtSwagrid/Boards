package boards.algebra

import boards.imports.games.{*, given}
import boards.graphics.Colour

abstract class Game (
  data: Game.Metadata
):
  
  def this (
    name: String = "",
    numPlayers: Seq[Int] = Seq(2),
    playerNames: Seq[String] = Seq.empty,
    playerColours: Seq[Colour] = Seq.empty,
  ) =
    this(Game.Metadata(
      if name.isEmpty then getClass.getSimpleName else name,
      numPlayers,
      if playerNames.isEmpty then (1 to numPlayers.max)
        .map(i => s"Player $i") else playerNames,
      if playerColours.isEmpty then Seq.fill(numPlayers.max)(Colour.White) else playerColours,
    ))
  
  export data.*
  
  protected type GameBoard = Kernel[Colour]
  protected val Board: GameBoard
  
  def setup(numPlayers: Int): InstantaneousState ?=> InstantaneousState
  
  def rules: Rule
  def initial(numPlayers: Int): InitialState =
    val genesis = InstantaneousState.initial(Board, numPlayers)
    GameState.initial(this, setup(numPlayers)(using genesis), rules)
  
  
object Game:
  
  def none: Game = new Game(name = "Game"):
    def setup(numPlayers: Int) = InstantaneousState.empty
    def rules = Rule.none
    val Board = Kernel.empty
  
  case class Metadata (
    name: String,
    numPlayers: Seq[Int],
    playerNames: Seq[String],
    playerColours: Seq[Colour],
  )
    
  case class PlayerData (
    name: String,
    colour: Colour,
  )