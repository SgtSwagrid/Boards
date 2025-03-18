package boards

import boards.dsl.meta.Game
import boards.games.*

object GameCatalogue:
  
  val all = Seq[Game] (
    Chess,
    Amazons,
    TicTacToe,
    Hex,
    Neutron,
    Breakthrough,
    Chaturanga,
    Clobber,
    Camelot,
  )
  
  val byName: Map[String, Game] =
    all.map(game => game.name -> game).toMap