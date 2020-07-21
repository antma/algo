package com.github.antma.cpalgo

object GF {
  def powmod(x: Int, k: Int, q: Int) = {
    var a = 1
    var b = x
    var p = k
    while (p > 0) {
      if ((p & 1) != 0) {
        a = ((a.toLong * b.toLong) % q).toInt
      }
      b = ((b.toLong * b.toLong) % q).toInt
      p = (p >> 1)
    }
    a
  }
}
