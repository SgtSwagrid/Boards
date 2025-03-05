package boards.dsl.meta

import boards.dsl.meta.Game.{Board, GameConfig, Property}
import boards.dsl.meta.PlayerRef.Player
import boards.dsl.rules.{Effect, Rule}
import boards.dsl.states.{GameState, HistoryState, InstantaneousState}
import boards.graphics.Colour
import boards.math.vector.Embedding

/** The base class for implementations of turn-based board games written in BoardLang.
  *
  * All games must extend this class.
  * Typically, you may wish to override values such as [[numPlayers]] or [[rules]], among others.
  */
abstract class Game:
  
  /** The human-readable name for this game. */
  val name: String = ""
  
  /** The allowable numbers of players who can play together.
    * Will assume by default that the game requires exactly [[players.size]] many players.
    *
    * Note: at least one of [[numPlayers]] or [[players]] must be overridden.
    */
  def numPlayers: Seq[Int] = Seq(players.size)
  
  /** The properties of each player.
    * Will assign boring names by default: Player 1, Player 2, ...
    *
    * Note: at least one of [[numPlayers]] or [[players]] must be overridden.
    */
  def players: Seq[Player] =
    (0 until numPlayers.maxOption.getOrElse(0))
      .map(i => Player(i, s"Player ${i + 1}", Colour.White))
    
  /** The properties of this game which may be configured by the players before starting. */
  val properties: Seq[Property] = Seq.empty
  
  protected def board: GameConfig ?=> Board = Embedding.empty
  
  /** Get the initial state of this game, given some configuration. */
  final def initial (config: GameConfig): GameState =
    given GameConfig = config
    GameState.initial(HistoryState.initial(InstantaneousState.initial(board)), rules)
  
  /** The rules for this game, which defines legal [[Input]]s and their corresponding [[Effect]]s.
    *
    * This rule only governs the very first turn of the game, and is not repeated automatically.
    * The [[Rule]] for a game is dynamic and changes after each [[Input]].
    *
    * For a typical game with a setup step followed by a main game loop,
    * this need not be overridden, and one should instead override [[setup]] and [[loop]].
    *
    * Defaults to: {{{setup |> loop}}}
    */
  def rules: GameConfig ?=> Rule = setup |> loop
  
  /** The setup phase for this game. Called by the default implementation of [[rules]]. */
  def setup: GameConfig ?=> Rule = Effect.identity
  
  /** The main game loop for this game.
    * Called by the default implementation of [[rules]].
    * This rule is not automatically repeated.
    * If you want it to repeat, you must use something like [[Rule.alternatingTurns]] explicitly.
    */
  def loop: GameConfig ?=> Rule = Effect.identity
  
object Game:
  
  type Board = Embedding
  
  /** An empty game with no players, board or behaviour. */
  def none: Game = new Game { override val numPlayers = Seq(0) }
  
  /** A custom property of a game which can be set in the configuration stage.
    * @param name The human-readable name of the property.
    * @param values The set of allowable values for this property.
    * @param default The default value, used if no alternative is provided.
    */
  case class Property (
    name: String,
    values: Seq[Int],
    default: Int,
  ):
    export values.{min, max}
    /** Get the value to which this property is set in this game. */
    def get(using config: GameConfig) =
      config.properties.getOrElse(name, default)
  
  /** The configuration of a game, specifying the number of players and the values of any custom properties.
    * Can only be changed before the game begins.
    * @param numPlayers The number of players who are participating in the game.
    * @param properties The values of any custom properties which have been set.
    */
  case class GameConfig (
    numPlayers: Int = 2,
    properties: Map[String, Int] = Map.empty,
  ):
    def apply(property: String): Int = properties(property)
    
  object GameConfig:
    def empty = GameConfig()