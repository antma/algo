package com.github.antma.cpalgo
class Graph(val nodes: Int, maxEdges: Int) {
  private[this] val first = Array.fill(nodes)(-1)
  private[this] val nxt   = Array.fill(maxEdges) { -1 }
  private[this] val v     = Array.ofDim[Int](maxEdges)
  private[this] var k     = 0
  def addEdge(i: Int, j: Int): Unit = {
    v(k) = j
    nxt(k) = first(i)
    first(i) = k
    k += 1
  }
  def foreach(i: Int, op: Int => Unit): Unit = {
    var k = first(i)
    while (k >= 0) {
      op(v(k))
      k = nxt(k)
    }
  }
}
