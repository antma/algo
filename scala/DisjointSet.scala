package com.github.antma.cpalgo

class DisjointSet (n: Int) {
  val p = (0 until n).toArray
  val h = Array.ofDim[Int] (n)
  def find_set (x: Int): Int = {
    if (p(x) == x) x
    else {
      p(x) = find_set (p(x))
      p(x)
    }
  }
  def merge (x: Int, y: Int): Boolean = {
    val i = find_set (x)
    val j = find_set (y)
    if (i == j) false
    else {
      if (h(i) < h(j)) {
        p(i) = j
      } else if (h(i) > h(j)) {
        p(j) = i
      } else {
        p(i) = j
        h(j) += 1
      }
      true
    }
  }
}
