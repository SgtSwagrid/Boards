import boards.GameImports.{*, given}
import boards.games.Chess

object Test extends App:
  
  val actions = Chess.initial.actions.toSeq
  
  
  
  println(Chess.setup.draw)
  
  actions
    .map:
      case Move(_, from, to) => (from, to)
    .groupBy(_(0))
    .map((from, action) => s"$from |-> ${action.map(_(1)).mkString(", ")}")
    .toSeq.sorted
    .foreach(println)
  
  
  
  Chess.initial.next
    .flatMap:
      case s: InterimState => s.next
    .flatMap:
      case s: InterimState => s.next
    .flatMap:
      case s: InterimState => s.next
    .flatMap:
      case s: InterimState => s.next
    .flatMap:
      case s: InterimState => s.next
    .flatMap:
      case s: InterimState => s.next
    .flatMap:
      case s: InterimState => s.next
    .flatMap:
      case s: InterimState => s.next
    .flatMap:
      case s: InterimState => s.next
    .flatMap:
      case s: InterimState => s.next
    .flatMap:
      case s: InterimState => s.next
    .flatMap:
      case s: InterimState => s.next
    .flatMap:
      case s: InterimState => s.next
    .flatMap:
      case s: InterimState => s.next
  .map(_.draw).map(_ + "\n").foreach(println)
  
  //println(kernel.draw)
  //println(kernel.positions.toSeq)
  //println(kernel.extent)