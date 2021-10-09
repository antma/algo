object RMQ {
  fun calcH(len: Int): Int = 31 - java.lang.Integer.numberOfLeadingZeros(len)
}

class IntRMQ(a: IntArray, private val op: (Int, Int) -> Int) {
  private val n = a.size
  private val d = ArrayList<IntArray>().run {
    var b = a
    var k = 1
    while (true) {
      add(b)
      val kk = k + k
      if (kk > n) break
      b = IntArray(n - kk + 1) { op(b[it], b[it + k]) }
      k = kk
    }
    toTypedArray()
  }
  //[u, v)
  fun reduce(u: Int, v: Int, h: Int): Int {
    val e = d[h]
    return op(e[u], e[v - (1 shl h)])
  }
}
