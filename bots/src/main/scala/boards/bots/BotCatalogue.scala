package boards.bots

import boards.dsl.meta.Game

object BotCatalogue:
  
  val all = Seq[Bot[Game]] (
    RandomBot,
    Minimax,
  )
  
  val byId: Map[String, Bot[Game]] =
    all.map(bot => bot.name -> bot).toMap