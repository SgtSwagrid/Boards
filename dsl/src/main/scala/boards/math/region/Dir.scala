package boards.math.region

import boards.imports.math.{*, given}
import boards.math.Number.*
import Vec.HasVecI

import scala.annotation.targetName

/** A collection of helpers for describing directions with `VecI` and `RegionI`. */
object Dir:
  
  /** The 4 orthogonal directions (left, right, down, up) in 2D. */
  val orthogonal: RegionI = orthogonal(2)
  /** The orthogonal directions in d-dimensions,
   * whereby there is exactly 1 non-zero component of magnitude 1. */
  def orthogonal(dim: Int = 2): RegionI =
    Metric.Manhattan.neighbours(VecI.zero(dim))
  
  /** The 4 diagonal directions in 2D. */
  val diagonal: RegionI = diagonal(2)
  /** All diagonal directions in d-dimensions,
   * whereby each component has magnitude at most 1, and more than 1 component is non-zero. */
  def diagonal(dim: Int = 2): RegionI =
    octagonal(dim) \ orthogonal(dim)
  
  /** The 8 orthogonal and diagonal directions in 2D. */
  val octagonal: RegionI = octagonal(2)
  /** All orthogonal and diagonal directions in d-dimensions,
   * whereby each component has magnitude at most 1. */
  def octagonal(dim: Int = 2): RegionI =
    Metric.Chebyshev.neighbours(VecI.zero(dim))
  
  /**
   * The positive and negative direction along the identified axis.
   * 
   * Equivalent to `VecI.axis(axis, dim) | -VecI.axis(axis, dim)`.
   */
  def axis(axis: Int, dim: Int = 2): RegionI =
    VecI.axis(axis, dim) | -VecI.axis(axis, dim)
  
  /** The 2D direction pointing to the left, i.e. VecI(-1, 0). */
  val left: VecI = VecI(-1, 0)
  /** The 2D direction pointing to the right, i.e. VecI(1, 0). */
  val right: VecI = VecI(1, 0)
  /** The 2D direction pointing downward, i.e. VecI(0, -1). */
  val down: VecI = VecI(0, -1)
  /** The 2D direction pointing upward, i.e. VecI(0, 1). */
  val up: VecI = VecI(0, 1)
  
  /** The 2 horizontal directions (left, right) in 2D. */
  val horizontal = Dir.left | Dir.right
  /** The 2 vertical directions (down, up) in 2D. */
  val vertical = Dir.down | Dir.up
  
  /** The 2D diagonal direction VecI(-1, -1). */
  val bottomLeft: VecI = Dir.down + Dir.left
  /** The 2D diagonal direction VecI(1, -1). */
  val bottomRight: VecI = Dir.down + Dir.right
  /** The 2D diagonal direction VecI(-1, 1). */
  val topLeft: VecI = Dir.up + Dir.left
  /** The 2D diagonal direction VecI(1, 1). */
  val topRight: VecI = Dir.up + Dir.right
  
  /** The 2 left-diagonal directions (topLeft, bottomLeft) in 2D. */
  val diagonallyLeft: RegionI = Dir.bottomLeft | Dir.topLeft
  /** The 2 right-diagonal directions (topRight, bottomRight) in 2D. */
  val diagonallyRight: RegionI = Dir.bottomRight | Dir.topRight
  /** The 2 down-diagonal directions (bottomLeft, bottomRight) in 2D. */
  val diagonallyDown: RegionI = Dir.bottomLeft | Dir.bottomRight
  /** The 2 up-diagonal directions (topLeft, topRight) in 2D. */
  val diagonallyUp: RegionI = Dir.topLeft | Dir.topRight
  
  val positiveDiagonal: RegionI = Dir.topRight | Dir.bottomLeft
  
  val negativeDiagonal: RegionI = Dir.topLeft | Dir.bottomRight
  
  val orthogonalPairs: Seq[RegionI] = Seq(horizontal, vertical)
  val diagonalPairs: Seq[RegionI] = Seq(positiveDiagonal, negativeDiagonal)
  val octagonalPairs: Seq[RegionI] = orthogonalPairs ++ diagonalPairs
  
  /**
   * Enumerates all the directions in (`steps.size`)-dimensional space such that
   * each provided step size integer appears as the absolute value of exactly one component of the direction vector.
   *
   * @example The knight in chess can move in the directions `Dir.knight(1, 2)`,
   *          because it moves 1 space in some direction and 2 spaces in the other.
   */
  def knight(steps: Int*): RegionI =
    val dirs =
      for
        dir <- steps.permutations.toSeq
        sign <- Seq.fill(dir.length)(Seq(Seq(-1), Seq(1)))
          .reduce((A, B) => for (a <- A; b <- B) yield a ++ b)
      yield Vec(dir*) * Vec(sign*)
    Region.from(dirs)
  
  def of(direction: HasVecI): VecI =
    direction.position.direction
  
  /**
   * Find the relative direction between `from` and `to`,
   * by dividing the difference by the GCD of its components.
   */
  def between(from: HasVecI, to: HasVecI): VecI =
    from.position.directionTo(to)