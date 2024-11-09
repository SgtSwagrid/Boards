package boards.imports

import boards.math.{Align, Dir, Ray, Region}

object math:
  
  export boards.math.{Number, Vec, Metric, Region, Ray, Dir, Align}
  export Vec.{VecI, VecF, given}
  export Region.{RegionI, given}
  export boards.math.Algebra.{*, given}
  export boards.math.Metric.EnumerableMetric