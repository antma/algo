import java.math.BigInteger

final class Fraction private constructor(val numerator: BigInteger, val denominator: BigInteger) {
  companion object {
    fun new(numerator: BigInteger, denominator: BigInteger): Fraction {
      if (denominator == BigInteger.ZERO) throw ArithmeticException("division by zero")
      else {
        val g = numerator.gcd (denominator) 
        val x = numerator / g
        val y = denominator / g
        return if (y < BigInteger.ZERO) Fraction(-x, -y) else Fraction(x, y)
      }
    }
  }
  operator fun plus (rhs: Fraction) = 
    new(numerator * rhs.denominator + rhs.numerator * denominator,
        denominator * rhs.denominator)
  fun inversed(): Fraction = when { 
    numerator < BigInteger.ZERO -> Fraction(-denominator, -numerator)
    numerator > BigInteger.ZERO -> Fraction(denominator, numerator)
    else -> throw ArithmeticException("division by zero")
  }
}
