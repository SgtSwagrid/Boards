package boards.dsl.meta

import Game.{Board, GameConfig}
import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.math.region.Region
import boards.math.region.RegionMap.RegionMapI
import boards.util.extensions.CollectionOps.contramap
import io.circe.Codec

import scala.annotation.targetName

abstract class Game (
  data: Game.Metadata
):
  
  def this (
    name: String = "",
    numPlayers: Seq[Int] = Seq(2),
    playerNames: Seq[String] = Seq.empty,
    playerColours: Seq[boards.graphics.Colour] = Seq.empty,
  ) =
    this(Game.Metadata(
      if name.isEmpty then ""/*getClass.getSimpleName*/ else name,
      numPlayers,
      if playerNames.isEmpty then (1 to numPlayers.max)
        .map(i => s"Player $i") else playerNames,
      if playerColours.isEmpty then Seq.fill(numPlayers.max)(Colour.White) else playerColours,
    ))
  
  export data.*
  
  protected val board: Board
  
  final def initial(config: GameConfig): GameState =
    GameState.initial(HistoryState.initial(InstantaneousState.initial(board, config)), rules)
  
  def rules: Rule
  
object Game:
  
  type Board = RegionMapI[boards.graphics.Colour]
  
  def none: Game = new Game(name = "Game"):
    def setup(config: GameConfig) = InstantaneousState.empty
    def rules = Cause.none
    val board = Region.empty.fill(Colour.White)
  
  case class Metadata (
    name: String,
    numPlayers: Seq[Int],
    playerNames: Seq[String],
    playerColours: Seq[boards.graphics.Colour],
  )
    
  case class PlayerData (
    name: String,
    colour: Colour,
  )
  
  case class GameConfig (
    numPlayers: Int,
  )