class ArraySet (_a: Array[Int]) {
  val a = _a
  val n = a.length
  def union (that: ArraySet): ArraySet = {
    val b = Array.ofDim[Int] (n + that.n);
    var i = 0
    var j = 0
    var k = 0
    while (i < n && j < that.n) {
      if (a(i) < that.a(j)) {
        b(k) = a(i)
        i += 1
        k += 1
      } else if (a(i) > that.a(j)) {
        b(k) = that.a(j)
        j += 1
        k += 1
      } else {
        b(k) = a(i)
        i += 1
        j += 1
        k += 1
      }
    }
    while (i < n) {
      b(k) = a(i)
      i += 1
      k += 1
    }
    while (j < that.n) {
      b(k) = that.a(j)
      j += 1
      k += 1
    }
    new ArraySet (b.slice (0, k))
  }
  private def binsearch (v: Int) = {
    var l = -1
    var r = n
    while (r - l > 1) {
      val m = (l + r) >> 1
      if (a(m) <= v) l = m else r = m
    }
    l
  }
  def upper (v: Int): Int = {
    val l = binsearch (v)
    if (l < 0) a(0)
    else if (l >= n) Int.MaxValue
    else if (a(l) > v) a(l)
    else if (l + 1 < n) a(l + 1)
    else Int.MaxValue
  }
  def lower (v: Int): Int = {
    val l = binsearch (v)
    if (l < 0) Int.MinValue
    else if (l >= n) a(n - 1)
    else if (a(l) < v) a(l)
    else if (l > 0) a (l - 1)
    else Int.MinValue
  }
  //[from, to]
  def count (from: Int, to: Int): Int = {
    var l = binsearch (from)
    if (l < 0 || a(l) != from) l += 1
    var r = binsearch (to) + 1
    r - l
  }
  override def toString = a.toList.toString
}
