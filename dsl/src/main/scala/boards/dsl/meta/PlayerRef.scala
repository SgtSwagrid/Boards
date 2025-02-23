package boards.dsl.meta

import Game.GameConfig
import boards.dsl.meta.PlayerRef.PlayerId
import boards.dsl.states.GameState.Outcome
import boards.dsl.states.GameState.Outcome.Winner
import boards.graphics.Colour
import boards.util.extensions.CollectionOps.contramap

object PlayerRef:
  
  opaque type PlayerId = Int
  
  type PlayerRef = PlayerId | Player
  
  extension (playerRef: PlayerRef) def playerId: PlayerId = playerRef match
    case player: Player => player.playerId
    case playerId: PlayerId => playerId
  
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
  
  object PlayerId:
    def apply(id: Int): PlayerId = id
    def initial: PlayerId = PlayerId(0)
  
  given Ordering[PlayerId] = Ordering.Int.contramap(_.toInt)
  
  case class Player (
    playerId: PlayerId,
    name: String,
    colour: Colour = Colour.White,
  )
  
  object Player:
    
    def apply (playerId: Int, name: String, colour: Colour = Colour.White): Player =
      new Player(PlayerId(playerId), name, colour)