package com.github.antma.cpalgo

final class DisjointSet(n: Int) {
  val p = Array.range(0, n)
  val h = Array.ofDim[Int](n)
  def findSet(x: Int): Int = {
    if (p(x) == x) x
    else {
      p(x) = findSet(p(x))
      p(x)
    }
  }
  def merge(x: Int, y: Int): Boolean = {
    val i = findSet(x)
    val j = findSet(y)
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
