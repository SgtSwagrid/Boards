package boards.util.extensions

object IntOps:
  
  private val ones      = Seq("", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX")
  private val tens      = Seq("", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC")
  private val hundreds  = Seq("", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM")
  private val thousands = Seq("", "M", "MM", "MMM")
  
  extension (n: Int)
    
    def toRomanNumeral: String =
      if n >= 4000 then throw new IllegalArgumentException("Number too big for Roman numeral.")
      thousands((n / 1000) % 10) ++
      hundreds((n / 100) % 10) ++
      tens((n / 10) % 10) ++
      ones(n % 10)