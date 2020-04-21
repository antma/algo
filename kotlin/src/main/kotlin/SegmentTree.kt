class IntSegmentTree(a: IntArray, private val op: (Int, Int) -> Int, private val empty: Int) {
  private val n = a.size
  private val t = IntArray(2 * n)
  init {
    a.copyInto (destination = t, destinationOffset = n)
    for (i in n-1 downTo 1) {
      val k = 2 * i
      t[i] = op(t[k], t[k+1])
    }
  }
  fun update(p: Int, v: Int) {
    var i = p + n
    t[i] = v
    while (i > 1) {
      val k = i ushr 1
      t[k] = op (t[i], t[i xor 1])
      i = k
    }
  }
  fun reduce (l: Int, r: Int): Int {
    var res = empty
    var i = l + n
    var j = r + n
    while (i < j) {
      if (0 != (i and 1)) res = op (res, t[i])
      if (0 != (j and 1)) res = op (t[j-1], res)
      i = (i + 1) ushr 1
      j = j ushr 1
    }
    return res
  }
}

class IntSegmentTreeSliceIncrement(val n: Int) {
  private val t = IntArray(2 * n) { 0 }
  fun update(l0: Int, r0: Int, v: Int) {
    if (l0 < r0) {
      var l = l0 + n
      var r = r0 + n
      while(l < r) {
        if (0 != l and 1) t[l++] += v
        l = l ushr 1
        if (0 != r and 1) t[--r] += v;
        r = r ushr 1
      }
    }
  }
  operator fun get(index: Int): Int {
    var res = 0
    var i = index + n
    while (i > 0) {
      res += t[i]
      i = i ushr 1
    }
    return res
  }
}
