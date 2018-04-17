object IntM {
  val q = 1000000007
  def fromInt (v: Int) = { val x = v % q; new IntM (if (x < 0) x + q else x) }
}

class IntM (val v: Int) {
  def + (rhs: IntM) = new IntM ((v + rhs.v) % IntM.q)
  def - (rhs: IntM) = new IntM ((v - rhs.v + IntM.q) % IntM.q)
  def * (rhs: IntM) = new IntM (((v.toLong * rhs.v.toLong) % IntM.q).toInt)
  def + (rhs: Int): IntM = this + (IntM.fromInt (rhs))
  def - (rhs: Int): IntM = this - (IntM.fromInt (rhs))
  def * (rhs: Int): IntM = this * (IntM.fromInt (rhs))
  override def toString = v.toString
}
