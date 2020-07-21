package com.github.antma.cpalgo

object Gcd {
  def gcdext(a: Int, b: Int): (Int, Int, Int) = {
    if (b == 0) (a, 1, 0)
    else {
      val t           = a / b
      val (res, y, x) = gcdext(b, a - t * b)
      (res, x, y - x * t)
    }
  }
}

object IntM {
  val q = 1000000007
  def fromInt(v: Int) = { val x = v % q; new IntM(if (x < 0) x + q else x) }
  val zero = new IntM(0)
  val one  = new IntM(1)
}

class IntM private (val v: Int) extends AnyVal {
  def inverse = IntM.fromInt(Gcd.gcdext(IntM.q, v)._3)
  def +(rhs: IntM) = {
    val z = v + rhs.v
    new IntM(if (z >= IntM.q) z - IntM.q else z)
  }
  def -(rhs: IntM) = new IntM(if (v >= rhs.v) v - rhs.v else v - rhs.v + IntM.q)
  def *(rhs: IntM) = new IntM(((v.toLong * rhs.v.toLong) % IntM.q).toInt)
  def /(rhs: IntM): IntM = {
    assert(rhs.v > 0)
    return this * rhs.inverse
  }
  def pow(exp: Int): IntM = {
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
  override def toString = v.toString
}
