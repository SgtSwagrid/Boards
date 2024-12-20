package boards.dsl.meta

import Game.GameConfig
import boards.util.extensions.CollectionOps.contramap

object PlayerId:
  
  opaque type PlayerId = Int
  
  extension (playerId: PlayerId)
    def toInt: Int = playerId
    def + (x: Int) (using config: GameConfig): PlayerId =
      PlayerId(Math.max(playerId + x, 0) % config.numPlayers)
    def - (x: Int) (using config: GameConfig): PlayerId =
      PlayerId(Math.max(playerId - x, 0) % config.numPlayers)
    def next(using GameConfig): PlayerId = playerId + 1
    def previous(using GameConfig): PlayerId = playerId - 1
  
  def apply(id: Int): PlayerId = id
  def initial: PlayerId = PlayerId(0)
  
  given Ordering[PlayerId] = Ordering.Int.contramap(_.toInt)