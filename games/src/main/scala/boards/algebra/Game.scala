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
  
  final def initial(config: GameConfig): InitialState =
    val genesis = InstantaneousState.initial(Board, config)
    GameState.initial(this, setup(config)(using genesis), rules)
  def setup(config: GameConfig): InstantaneousState ?=> InstantaneousState
  
  def rules: Rule
  
object Game:
  
  def none: Game = new Game(name = "Game"):
    def setup(config: GameConfig) = InstantaneousState.empty
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
  
  case class GameConfig (
    numPlayers: Int,
  )
  
  opaque type PlayerId = Int
  
  object PlayerId:
    def apply(id: Int): PlayerId = id
    
  extension (id: PlayerId)
    def + (x: Int)(using config: GameConfig): PlayerId = (id + x) % config.numPlayers
    def - (x: Int)(using config: GameConfig): PlayerId = (id - x) % config.numPlayers
    def next(using GameConfig): PlayerId = id + 1
    def prev(using GameConfig): PlayerId = id - 1
    def toInt: Int = id