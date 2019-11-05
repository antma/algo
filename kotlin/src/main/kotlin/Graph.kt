class Graph(val nodes: Int, maxEdges: Int) {
  val first = IntArray(nodes) { -1 }
  val nxt = IntArray(maxEdges) { -1 }
  val v = IntArray(maxEdges)
  var k = 0
  fun addEdge(i: Int, j: Int) {
    v[k] = j
    nxt[k] = first[i]
    first[i] = k
    ++k
  }
  inner class AdjacentVerticesIterator(var k: Int) : Iterator<Int> {
    override fun hasNext() = k >= 0
    override fun next(): Int {
      val r = v[k]
      k = nxt[k]
      return r
    }
  }
  fun adjacentVerticesIterator(i: Int) = AdjacentVerticesIterator(first[i])
  inline fun adjacentVerticesForeach(i: Int, op: (Int) -> Unit) {
    var k = first[i]
    while (k >= 0) {
      op (v[k])
      k = nxt[k]
    }
  }
}

fun Graph.hasCycle(): Boolean {
  val u = IntArray(nodes)
  val v = IntArray(nodes)
  var t = 0
  fun go(i: Int): Boolean {
    u[i] = ++t
    adjacentVerticesForeach (i) { j ->
      if (u[j] == 0) {
        if (go(j)) return true
      } else if (u[j] < u[i] && v[j] == 0) {
        return true
      }
    }
    v[i] = ++t
    return false
  }
  for(i in 0 until nodes) {
    if (u[i] == 0) {
      if (go(i)) return true
    }
  }
  return false
}
