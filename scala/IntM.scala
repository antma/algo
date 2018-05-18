object Gcd {
  def gcdext (a: Int, b: Int): (Int, Int, Int) = {
    if (b == 0) (a, 1, 0)
    else {
      val t = a / b
      val (res, y, x) = gcdext (b, a - t * b);
      (res, x, y - x * t)
    }
  }
}

object IntM {
  val q = 1000000007
  def fromInt (v: Int) = { val x = v % q; new IntM (if (x < 0) x + q else x) }
  val zero = new IntM (0)
  val one = new IntM (1)
}

class IntM (val v: Int) {
  def inverse = Gcd.gcdext (IntM.q, v)._3
  def + (rhs: IntM) = new IntM ((v + rhs.v) % IntM.q)
  def - (rhs: IntM) = new IntM ((v - rhs.v + IntM.q) % IntM.q)
  def * (rhs: IntM) = new IntM (((v.toLong * rhs.v.toLong) % IntM.q).toInt)
  def / (rhs: IntM): IntM = {
    assert (rhs.v > 0)
    return this * rhs.inverse
  }
  def + (rhs: Int): IntM = this + (IntM.fromInt (rhs))
  def - (rhs: Int): IntM = this - (IntM.fromInt (rhs))
  def * (rhs: Int): IntM = this * (IntM.fromInt (rhs))
  def / (rhs: Int): IntM = this / (IntM.fromInt (rhs))
  def pow (exp: Int): IntM = {
    var a = IntM.one
    var b = this
    var p = exp
    while (p > 0) {
      if ((p & 1) != 0) {
        a = a * b
      }
      b = b * b
      p = p >> 1
    }
    a
  }
  def equals (rhs: IntM) = v == rhs.v
  override def toString = v.toString
}
