package boards.dsl.meta

import Game.GameConfig
import boards.dsl.states.GameState.Outcome
import boards.dsl.states.GameState.Outcome.Winner
import boards.util.extensions.CollectionOps.contramap

object PlayerId:
  
  opaque type PlayerId = Int
  
  extension (playerId: PlayerId)
    def toInt: Int = playerId
    def + (x: Int) (using config: GameConfig): PlayerId =
      PlayerId(Math.max(playerId + x, 0) % config.numPlayers)
    def - (x: Int) (using config: GameConfig): PlayerId =
      PlayerId(Math.max(playerId - x, 0) % config.numPlayers)
    def next(using config: GameConfig): PlayerId =
      (playerId + 1) % config.numPlayers
    def previous(using config: GameConfig): PlayerId =
      (playerId + config.numPlayers - 1) % config.numPlayers
    def wins: Outcome = Winner(playerId)
  
  def apply(id: Int): PlayerId = id
  def initial: PlayerId = PlayerId(0)
  
  given Ordering[PlayerId] = Ordering.Int.contramap(_.toInt)