package boards

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.algebra.shortcuts.{*, given}
import boards.games.Chess
import boards.games.Chess.Rook

object Test extends App:
  
  /*val s0 = Chess.initial(GameConfig(2))
  println(s0)
  println(s0.ruleOption.get.toString(s0))
  println(s0.now.piecesByPos)
  println(s0.actions.toSeq)
  println(s0.actionOption)
  
  println("======")
  
  val s2 = s0.next.next()///*.withRule(Transformer.insert(PlayerId(0))(Rook -> VecI(1, 1)))*/.flattenFutureSkips
  println(s2)
  println(s2.ruleOption.get.toString(s2))
  println(s2.now.piecesByPos)
  println(s2.actions.toSeq)
  println(s2.actionOption)
  
  println("======")
  
  val s3 = s2.next.toSeq.head
  println(s3)
  println(s3.ruleOption.get.toString(s3))
  println(s3.now.piecesByPos)
  println(s3.actions.toSeq)
  println(s3.actionOption)
  
  println("======")
  
  val s1 = s0.takeAction(Skip).get
  println(s1)
  println(s1.ruleOption.get.toString(s1))
  println(s1.now.piecesByPos)
  println(s1.actions.toSeq)
  println(s1.actionOption)*/
  
  val i = Iterator.iterate(Chess.initial(GameConfig(2))): state =>
    println(state)
    //println(state.ruleOption.get.toString(state))
    println(state.now.piecesByPos)
    println(state.actions.toSeq)
    println(state.actionOption)
    println(state.next.toSeq.map(s => s"${s.actionOption} -> ${s.now.piecesByPos}"))
    println("================================")
    state.next.next()
    
    
  i.take(10).toSeq