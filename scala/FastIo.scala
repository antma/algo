package org.github.antma.cpalgo

import scala.annotation.tailrec
import scala.collection.mutable.StringBuilder

object FastScanner {
  val zero = 0.toByte
  val cr = '\r'.toByte
  val nl = '\n'.toByte
  val BUFFER_SIZE = 0x10000
}

class FastScanner(private val input: java.io.InputStream) {
  private val b = new Array[Byte](FastScanner.BUFFER_SIZE)
  private var n = 0
  private var pos = 0
  private var eof = false
  private def read ():Boolean = {
    if (eof) {
      return false
    }
    pos = 0
    n = input.read(b)
    if (n < 0) {
      eof = true
    }
    n > 0
  }
  private def nextByte(): Byte = {
    if (pos >= n && !read ()) {
      0
    } else {
      val r = b(pos)
      pos += 1
      r
    }
  }
  @tailrec
  private def skipBlanks(): Byte = {
    val b = nextByte()
    if(b > 32 || b < 1) b else skipBlanks()
  }
  final def nextToken(k: Int = -1): String = {
    val sb = if(k >= 0) new StringBuilder(k) else new StringBuilder()
    var b = skipBlanks()
    assert(b > 0)
    while(b > 32) {
      sb.append(b.toChar)
      b = nextByte()
    }
    sb.toString
  }
  final def nextDouble(): Double = {
    @tailrec
    def g(t: Double, x: Double): Double = {
      val b = nextByte ()
      if (b <= 32) t * x else g(10 * t + (b - 48), x * 0.1)
    }
    @tailrec
    def f(t: Double): Double = {
      val b = nextByte ()
      if(b == 46) g(t, 1.0)
      else if (b <= 32) t else f(10 * t + (b - 48))
    }
    val b = skipBlanks()
    require(b > 0)
    if (b < 48) -f(0) else f(b - 48)
  }
  final def nextInt():Int = {
    val b = skipBlanks()
    @tailrec
    def f(t: Int): Int = {
      val b = nextByte ()
      if (b <= 32) t else f(10 * t + (b - 48))
    }
    require(b > 0)
    if (b < 48) -f(0) else f(b - 48)
  }
  final def nextLong():Long = {
    val b = skipBlanks()
    @tailrec
    def f(t: Long): Long = {
      val b = nextByte ()
      if (b <= 32) t else f(10 * t + (b - 48))
    }
    require(b > 0)
    if (b < 48) -f(0) else f(b - 48)
  }
}
