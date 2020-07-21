package com.github.antma.cpalgo

import scala.collection.mutable.BitSet

class PrimeTable(n: Int) { //finding primes < n
  val half = n / 2
  val c = {
    val m = Math.floor(Math.sqrt(n) * 0.5).toInt
    val b = new BitSet(half)
    b += 0
    //b(k) if 2 * k + 1 is composite number
    for (i <- 1 to m) {
      if (!b(i)) {
        val step  = 2 * i + 1
        val start = 2 * i * (i + 1)
        val k     = half / step
        b ++= (start to (start + k * step) by step)
      }
    }
    b
  }
  def primes = if (n <= 2) Nil else 2 +: (1 until half).filterNot(i => c(i)).map(i => 2 * i + 1).toList
}
