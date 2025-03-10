package boards.bots

import boards.dsl.meta.Game
import boards.dsl.rules.Input
import boards.dsl.states.GameState
import scala.util.Random

object Minimax extends Bot[Game]:
  
  type StateMetadata = Nothing
  
  val name: String = "Minimax"
  
  def choose (state: GameState): Input =
    state.inputs(Random(state.config.seed ^ state.turnId).nextInt(state.next.size))
  
  def evaluate (state: GameState): Float = 0.0F
