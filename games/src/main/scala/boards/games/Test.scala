package boards.games

import boards.dsl.meta.Game.GameConfig
import boards.imports.math.{*, given}
import boards.imports.games.{*, given}
import boards.dsl.shortcuts.{*, given}

object Test extends App:
  
  val initial = Chess.initial(GameConfig(2))
  
  