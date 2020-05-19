class IntFenwickTree(val n: Int) {
  private val a = IntArray(n)
  fun update(x: Int, v: Int) {
    assert(x < n)
    var i = x
    while (i < n) {
      a[i] += v
      i = i or (i + 1)
    }
  }
  fun reduce(x: Int): Int {
    assert(x < n)
    var r = 0
    var i = x
    while (i >= 0) {
      r += a[i]
      i = (i and (i + 1)) - 1
    }
    return r
  }
}
