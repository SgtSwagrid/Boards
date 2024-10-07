package boards

import boards.algebra.Game
import boards.games.*

object Games:
  
  val all = Seq (
    Chess,
    Chaturanga,
  )
  
  val byName: Map[String, Game] =
    all.map(g => g.data.name -> g).toMap