package boards

import boards.dsl.meta.Game
import boards.imports.games.{*, given}
import boards.games.*

object Catalogue:
  
  val all = Seq[Game] (
    Chess,
    Amazons,
  )
  
  val byName: Map[String, Game] =
    all.map(game => game.name -> game).toMap