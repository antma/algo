package com.github.antma.cpalgo

import scala.annotation.tailrec
import scala.collection.mutable.StringBuilder

object FastIO {
  private val BUFFER_SIZE = 0x100000
  private val cr          = '\r'.toInt
  private val nl          = '\n'.toInt
  class Scanner private[FastIO] (input: java.io.InputStream) {
    private val r = new java.io.BufferedInputStream(input, BUFFER_SIZE)
    @tailrec
    private def skipBlanks(): Int = {
      val b = r.read()
      if (b > 32 || b < 0) b else skipBlanks()
    }
    final def nextLine(k: Int = -1): Option[String] = {
      val sb = if (k >= 0) new StringBuilder(k) else new StringBuilder()
      @tailrec
      def loop(): Option[String] = {
        val b = r.read()
        if (b < 0) {
          if (sb.isEmpty) None else Some(sb.result())
        } else if (b == nl) {
          Some(sb.result())
        } else if (b == cr) {
          require(r.read() == nl, "expected newline after carrage return")
          Some(sb.result())
        } else {
          sb.append(b.toChar)
          loop()
        }
      }
      loop()
    }
    final def nextToken(k: Int = -1): String = {
      val sb = if (k >= 0) new StringBuilder(k) else new StringBuilder()
      var b  = skipBlanks()
      require(b >= 0, "unexpected EOF")
      while (b > 32) {
        sb.append(b.toChar)
        b = r.read()
      }
      sb.toString
    }
    final def nextDouble(): Double = {
      @tailrec
      def g(t: Double, x: Double): Double = {
        val b = r.read()
        if (b <= 32) t * x else g(10 * t + (b - 48), x * 0.1)
      }
      @tailrec
      def f(t: Double): Double = {
        val b = r.read()
        if (b == 46) g(t, 1.0)
        else if (b <= 32) t
        else f(10 * t + (b - 48))
      }
      val b = skipBlanks()
      require(b >= 0, "unexpected EOF")
      if (b < 48) -f(0) else f(b - 48)
    }
    final def nextInt(): Int = {
      val b = skipBlanks()
      @tailrec
      def f(t: Int): Int = {
        val b = r.read()
        if (b <= 32) t else f(10 * t + (b - 48))
      }
      require(b >= 0, "unexpected EOF")
      if (b < 48) -f(0) else f(b - 48)
    }
    final def nextLong(): Long = {
      val b = skipBlanks()
      @tailrec
      def f(t: Long): Long = {
        val b = r.read()
        if (b <= 32) t else f(10 * t + (b - 48))
      }
      require(b >= 0, "unexpected EOF")
      if (b < 48) -f(0) else f(b - 48)
    }
  }
  final def newScanner(input: java.io.InputStream) = new Scanner(input)
  final def newWriter(output: java.io.OutputStream) = {
    val osw = new java.io.OutputStreamWriter(output, java.nio.charset.StandardCharsets.US_ASCII)
    val bw  = new java.io.BufferedWriter(osw, BUFFER_SIZE)
    new java.io.PrintWriter(bw, false)
  }
}
