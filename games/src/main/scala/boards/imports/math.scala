package boards.imports

object math:
  
  export boards.math.{Number, Vec, Metric}
  export boards.math.Vec.{VecI, VecF, given}
  export boards.math.kernel.{Kernel, Ray, Dir, Align}
  export boards.math.kernel.Dir.given
  export boards.math.kernel.Kernel.given
  export boards.math.Algebra.{*, given}
  export boards.math.Metric.EnumerableMetric