object IntM {
  val q = 1000000007
  def fromInt (v: Int) = { val x = v % q; new IntM (if (x < 0) x + q else x) }
}

class IntM (val v: Int) {
  def + (rhs: IntM) = new IntM ((v + rhs.v) % IntM.q)
  def - (rhs: IntM) = new IntM ((v - rhs.v + IntM.q) % IntM.q)
  def * (rhs: IntM) = new IntM (((v.toLong * rhs.v.toLong) % IntM.q).toInt)
  def / (rhs: IntM): IntM = {
    assert (rhs.v > 0)
    val i = rhs.pow (IntM.q - 2)
    return this * i
  }
  def + (rhs: Int): IntM = this + (IntM.fromInt (rhs))
  def - (rhs: Int): IntM = this - (IntM.fromInt (rhs))
  def * (rhs: Int): IntM = this * (IntM.fromInt (rhs))
  def / (rhs: Int): IntM = this / (IntM.fromInt (rhs))
  def pow (exp: Int): IntM = {
    var a = new IntM (1)
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
