package boards

import boards.dsl.meta.Game
import boards.games.*

object Catalogue:
  
  val all = Seq[Game] (
    Chess,
    Amazons,
    TicTacToe,
    HexChess,
  )
  
  val byName: Map[String, Game] =
    all.map(game => game.name -> game).toMap