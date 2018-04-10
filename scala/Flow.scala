
//0 - sink, (n - 1) - target
class Flow (_n: Int) {
  type EdgeCost = Int
  val n = _n
  val edges = Array.ofDim[List[Int]] (n)
  val c = Array.ofDim[EdgeCost] (n, n)
  val f = Array.ofDim[EdgeCost] (n, n)
  val h = Array.fill (n) (0)
  val e = Array.fill (n) (0)
  val current = Array.ofDim[List[Int]] (n)

  def addEdge (i: Int, j: Int, w: EdgeCost) = {
    c(i)(j) = w
  }

  private def buildEdges () = {
    for (i <- 0 until n) {
      edges(i) = (0 until n).filter (j => (c(i)(j) > 0) || (c(j)(i) > 0)).toList
    }
  }

  private def initPreflow () = {
    h(0) = n
    for (i <- edges(0)) {
      f(0)(i) = c(0)(i)
      f(i)(0) = -c(0)(i)
      e(i) = c(0)(i)
    }
  }
  private def push (i: Int, j: Int) = {
    val d = e(i).min (c(i)(j) - f(i)(j))
    f(i)(j) += d
    f(j)(i) = -f(i)(j)
    e(i) -= d
    e(j) += d
  }
  private def lift (i: Int) = {
    val l = edges(i).filter (j => f(i)(j) < c(i)(j))
    h(i) = 1 + l.map (j => h(j)).min
  }
  private def discharge (i: Int) = {
    while (e(i) > 0) {
      val l = current(i)
      if (l.isEmpty) {
        lift (i)
        current(i) = edges(i)
      } else {
        val j = current(i).head
        if (f(i)(j) < c(i)(j) && h(i) == h(j) + 1) push (i, j)
        else current(i) = current(i).tail
      }
    }
  }

  def maxFlow () = {
    buildEdges ()
    initPreflow ()
    val next = Array.ofDim[Int] (n)
    val prev = Array.ofDim[Int] (n)
    def addLink (u: Int, v: Int) {
      next (u) = v
      prev (v) = u
    }
    def len (i: Int): Int = if (i == 0) 0 else 1 + len (next(i))
    for (i <- 1 to n - 2) {
      next(i) = i + 1
      prev(i) = i - 1
      current(i) = edges(i)
    }
    next(0) = 1
    addLink (n - 2, 0)
    assert (len (next(0)) == n - 2)
    var i = 1
    while (i > 0) {
      val old_height = h(i)
      discharge (i)
      if (h(i) > old_height) {
        //move to front
        addLink (prev(i), next(i))
        addLink (i, next(0))
        addLink (0, i)
      }
      assert (len (next(0)) == n - 2)
      i = next(i)
    }
    (1 to n-1).map (i => f(0)(i)).sum
  }
}
