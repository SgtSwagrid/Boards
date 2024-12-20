package boards.imports

import boards.math.region.{Align, Dir, Metric, Ray, Region, Vec}

object math:
  
  export boards.math.Number
  export boards.math.region.{Vec, Metric, Region, Ray, Dir, Align}
  export Vec.{VecI, VecF, given}
  export Region.{RegionI, given}
  export boards.math.Algebra.{*, given}
  export Metric.EnumerableMetric