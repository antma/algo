class IntSegmentTree(a: IntArray, private val op: (Int, Int) -> Int, private val empty: Int) {
  private val n = a.size
  private val t = IntArray(2 * n)
  init {
    a.copyInto (destination = t, destinationOffset = n)
    for (i in (n-1) downTo 0) {
      val k = 2 * i
      t[i] = op(t[k], t[k+1])
    }
  }
  fun update(p: Int, v: Int) {
    val p0 = p + n
    t[p0] = v
    tailrec fun loop (i: Int) {
      if (i > 1) {
        val k = i ushr 1
        t[k] = op (t[i], t[i xor 1])
        loop (k)
      }
    }
    loop (p0)
  }
  fun reduce (l: Int, r: Int): Int {
    var res = empty
    tailrec fun loop (i: Int, j: Int) {
      if (i < j) {
        if (0 != (i and 1)) res = op (res, t[i])
        if (0 != (j and 1)) res = op (t[j-1], res)
        loop ((i + 1) ushr 1, j ushr 1)
      }
    }
    loop (l + n, r + n)
    return res
  }
}
