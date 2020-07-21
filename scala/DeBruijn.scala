package com.github.antma.cpalgo

class DeBruijn {
  val db = 0x218a392cd3d5dbfL
  val t  = Array.ofDim[Int](64)
  for (k <- 0 to 63) t(((db << k) >>> 58).toInt) = k
  def lookup(x: Long) = t(((x * db) >>> 58).toInt)
}

class DeBruijn32 {
  val db = 0x4653adf
  val t  = Array.ofDim[Int](32)
  for (k <- 0 to 31) t((db << k) >>> 27) = k
  def lookup(x: Int) = t(((x * db) >>> 27).toInt)
}
