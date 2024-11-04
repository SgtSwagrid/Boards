package boards.util.extensions

import boards.util.structures.Monadic
import scala.annotation.{tailrec, targetName}
import scala.collection.IterableOps
import scala.collection.immutable.*
import scala.math.Ordered.orderingToOrdered

object CollectionOps:
  
  type Collection[T[_], X] = IterableOps[X, T, T[X]]

  type MultiMap[K, +X] = Map[K, Seq[X]]
  type SortedMultiMap[K, +X] = SortedMap[K, Seq[X]]
  
  extension [X] (seq: Seq[X])
    
    def takeUntil(f: X => Boolean): Seq[X] =
      seq.takeWhile(!f(_))
      
    def takeTo(f: X => Boolean): Seq[X] =
      seq.takeUntil(f) ++ dropUntil(f).headOption
      
    def dropUntil(f: X => Boolean): Seq[X] =
      seq.dropWhile(!f(_))
      
    def dropTo(f: X => Boolean): Seq[X] =
      dropUntil(f).drop(1)
  
  extension [X, T[Z] <: Collection[T, Z]] (collection: T[X])
    
    def traverse
      [Y, M[Z] <: Monadic[M, Z]]
      (f: X => M[Y])
      (using pure: [Z] => Z => M[Z])
    : M[T[Y]] =
      val empty = collection.empty.map(_.asInstanceOf[Y])
      collection.foldLeft[M[T[Y]]](pure(empty)):
        (m, x) => m.flatMap(m => f(x).map(y => m ++ Seq(y)))
      
    def chain
      [Y, M[Z] <: Monadic[M, Z]]
      (f: X => M[Y])
      (using pure: [Z] => Z => M[Z])
    : M[Y] =
      collection.traverse(f).map(_.last)
      
  extension [X, Y] (order: Ordering[X])
    def contramap(f: Y => X): Ordering[Y] =
      (y1: Y, y2: Y) => order.compare(f(y1), f(y2))

  /*extension [K : Ordering, X] (m1: SortedMultiMap[K, X])

    @targetName("merge") @tailrec
    def +++ [Y] (
      m2: SortedMultiMap[K, Y],
      prefix: SortedMultiMap[K, X | Y] = m1.empty
    ): SortedMultiMap[K, X | Y] =

      (m1, m2) match
        case ((t1 -> x) +: xs, (t2 -> y) +: ys) if t1 == t2 =>
          xs +++ (ys, prefix + (t1 -> (x ++ y)))
        case ((t1 -> _) +: _, (t2 -> _) +: _) =>
          val max = if t1 >= t2 then t1 else t2
          m1.rangeFrom(max) +++ (m2.rangeFrom(max),
            prefix ++ m1.rangeUntil(max) ++ m2.rangeUntil(max))
        case _ => prefix ++ m1 ++ m2

  @targetName("prepend") object +: :

    def unapply[X, T[A] <: SortedSetOps[A, ? <: T, T[A]]]
      (x: T[X]): Option[(X, T[X])] =
      x.headOption.map((_, x.tail))

    def unapply[K, X, T[A, +B] <: Map[A, B] & SortedMapOps[A, B, T, T[A, B]]]
      (x: T[K, X]): Option[((K, X), T[K, X])] =
      x.headOption.map((_, x.tail))
  
  @targetName("append") object :+ :

    def unapply[X, T[A] <: SortedSetOps[A, ? <: T, T[A]]]
      (x: T[X]): Option[(T[X], X)] =
      x.lastOption.map((x.init, _))

    def unapply[K, X, T[A, +B] <: Map[A, B] & SortedMapOps[A, B, T, T[A, B]]]
      (x: T[K, X]): Option[(T[K, X], (K, X))] =
      x.lastOption.map((x.init, _))*/