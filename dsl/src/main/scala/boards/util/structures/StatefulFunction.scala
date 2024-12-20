package boards.util.structures

case class StatefulFunction [S, +X] (
  private val f: S => (X, S)
) extends Monadic[[Z] =>> StatefulFunction[S, Z], X]:

  def apply(s: S): X = f(s)(0)

  def map[Y](g: X => Y): StatefulFunction[S, Y] =
    StatefulFunction: s1 =>
      val (x, s2) = f(s1)
      (g(x), s2)

  def flatMap[Y](g: X => StatefulFunction[S, Y]): StatefulFunction[S, Y] =
    StatefulFunction: s1 =>
      val (x, s2) = f(s1)
      g(x).f(s2)

  def zip[Y](s: StatefulFunction[S, Y]): StatefulFunction[S, (X, Y)] =
    StatefulFunction: s1 =>
      val (x, s2) = f(s1)
      val (y, s3) = s.f(s2)
      ((x, y), s3)

object StatefulFunction:

  def identity[S]: StatefulFunction[S, Unit] = StatefulFunction(s => ((), s))
  def constant[S, X](x: X): StatefulFunction[S, X] = StatefulFunction(s => (x, s))

  def read[S]: StatefulFunction[S, S] = StatefulFunction(s => (s, s))
  def write[S](s: S): StatefulFunction[S, Unit] = StatefulFunction(_ => ((), s))
  def transform[S](f: S => S): StatefulFunction[S, Unit] = StatefulFunction(s => ((), f(s)))

  given [S]: ([X] => X => StatefulFunction[S, X]) = [X] => (x: X) => constant[S, X](x)