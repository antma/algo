package org.github.antma.cpalgo
object Integration {
  def simpson(a: Double, b: Double, eps: Double, f : Double => Double): Double = {
    val fa = f(a)
    val fb = f(b)
    val avg = (fa + fb) * 0.5
    val l = (b - a)
    val l3 = l / 3.0
    def calc(n: Int, s1: Double, s2: Double): Double = {
      (l3 / n) * (avg + s1 + 2.0 * s2)
    }
    def loop(n: Int, s1: Double, pi: Double): Double = {
      val step = l / n
      val s2 = (0 until n).map {k => f(a + (k + 0.5) * step) }.sum 
      val i = calc(n, s1, s2)
      if((i - pi).abs < eps) i
      else loop(2 * n, s1 + s2, i)
    }
    loop(2, f((a + b) * 0.5), 0.0)
  }
}

