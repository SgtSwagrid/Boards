package boards.util.structures

trait Monadic[M[_] <: Monadic[M, ?], +X]:

  def map[Y](f: X => Y): M[Y]

  def flatMap[Y](f: X => M[Y]): M[Y]

  def foreach(f: X => Unit): Unit = map(f)