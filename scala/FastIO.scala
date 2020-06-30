package org.github.antma.cpalgo

import scala.annotation.tailrec
import scala.collection.mutable.StringBuilder

private object FastScanner {
  val cr = '\r'.toByte
  val nl = '\n'.toByte
  val BUFFER_SIZE = 0x10000
}

class FastScanner(input: java.io.InputStream) {
  private val r = new java.io.BufferedInputStream(input, FastScanner.BUFFER_SIZE)
  @tailrec
  private def skipBlanks(): Int = {
    val b = r.read()
    if(b > 32 || b < 0) b else skipBlanks()
  }
  final def nextToken(k: Int = -1): String = {
    val sb = if(k >= 0) new StringBuilder(k) else new StringBuilder()
    var b = skipBlanks()
    require(b >= 0, "unexpected EOF")
    while(b > 32) {
      sb.append(b.toChar)
      b = r.read()
    }
    sb.toString
  }
  final def nextDouble(): Double = {
    @tailrec
    def g(t: Double, x: Double): Double = {
      val b = r.read ()
      if (b <= 32) t * x else g(10 * t + (b - 48), x * 0.1)
    }
    @tailrec
    def f(t: Double): Double = {
      val b = r.read ()
      if(b == 46) g(t, 1.0)
      else if (b <= 32) t else f(10 * t + (b - 48))
    }
    val b = skipBlanks()
    require(b >= 0, "unexpected EOF")
    if (b < 48) -f(0) else f(b - 48)
  }
  final def nextInt():Int = {
    val b = skipBlanks()
    @tailrec
    def f(t: Int): Int = {
      val b = r.read ()
      if (b <= 32) t else f(10 * t + (b - 48))
    }
    require(b >= 0, "unexpected EOF")
    if (b < 48) -f(0) else f(b - 48)
  }
  final def nextLong():Long = {
    val b = skipBlanks()
    @tailrec
    def f(t: Long): Long = {
      val b = r.read ()
      if (b <= 32) t else f(10 * t + (b - 48))
    }
    require(b >= 0, "unexpected EOF")
    if (b < 48) -f(0) else f(b - 48)
  }
}
