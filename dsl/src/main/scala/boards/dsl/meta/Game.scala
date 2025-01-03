package boards.dsl.meta

import Game.{Board, GameConfig, Property}
import boards.dsl.meta.PlayerRef.Player
import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.math.region.{Region, RegionMap}
import boards.math.region.RegionMap.RegionMapI
import boards.util.extensions.CollectionOps.contramap
import io.circe.Codec

import scala.annotation.targetName

abstract class Game:
  
  val name: String = ""
  val numPlayers: Seq[Int] = Seq(2)
  val players: Seq[Player] =
    (0 until numPlayers.max).map(i => Player(i, s"Player ${i + 1}", Colour.White))
  val properties: Seq[Property] = Seq.empty
  
  protected val board: Board = RegionMap.empty
  
  final def initial(config: GameConfig): GameState =
    GameState.initial(HistoryState.initial(InstantaneousState.initial(board, config)), rules)
  
  def rules: Rule = setup |> loop
  def setup: Rule = Effect.identity
  def loop: Rule = Effect.identity
  
object Game:
  
  type Board = RegionMapI[boards.graphics.Colour]
  
  def none: Game = new Game {}
  
  case class Property (
    name: String,
    values: Seq[Int],
    default: Int,
  ):
    export values.{min, max}
    def get(using config: GameConfig) =
      config.properties.getOrElse(name, default)
  
  case class GameConfig (
    numPlayers: Int = 2,
    properties: Map[String, Int] = Map.empty,
  ):
    def apply(property: String): Int = properties(property)