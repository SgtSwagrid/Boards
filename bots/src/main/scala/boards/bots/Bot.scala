package boards.bots

import boards.dsl.meta.Game
import boards.dsl.rules.Input
import boards.dsl.states.GameState
import scala.util.Random

trait Bot [-G <: Game]:
  
  type StateMetadata
  
  val name: String
  
  def choose (state: GameState): Input
  
  def evaluate (state: GameState): Float