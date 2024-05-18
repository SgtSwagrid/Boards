package util.extensions

object Conversions:
  
  given [A, B, C](using convert: Conversion[A, B]): Conversion[(A, C), (B, C)] with
    def apply(x: (A, C)): (B, C) = (convert(x(0)), x(1))
  
  given [A, B, C](using convert: Conversion[B, C]): Conversion[(A, B), (A, C)] with
    def apply(x: (A, B)): (A, C) = (x(0), convert(x(1)))
  
  given [A, B, C, D] (
    using convertLeft: Conversion[A, B], convertRight: Conversion[C, D]
  ): Conversion[(A, C), (B, D)] with
    def apply(x: (A, C)): (B, D) = (convertLeft(x(0)), convertRight(x(1)))
  
  given [A, B, C](using convertLeft: Conversion[A, B], convertRight: Conversion[B, C]): Conversion[A, C] with
    def apply(x: A): C = convertRight(convertLeft(x))