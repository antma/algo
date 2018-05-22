//Origin: http://codeforces.com/blog/entry/18051

import annotation.tailrec

//cummutative lazy propagation segment tree (increment modifications, queries on mininum/maximum)
class CLPSegmentTree[T: reflect.ClassTag] (n: Int, add: (T, T) => T, max: (T, T) => T, empty: T, zero: T) {
  private val t = Array.fill (2 * n)(zero)
  private val d = Array.fill (n)(zero)
  private val h = (0 to 30).find (i => n < (1 << i)).head
  private def apply (p: Int, v: T) {
    t(p) = add (t(p), v)
    if (p < n) d(p) = add (d(p), v)
  }
  @tailrec private def build (p: Int) {
    if (p > 1) {
      val q = p >>> 1
      t(q) = add (max (t(q<<1), t((q<<1) + 1)), d(q))
      build (q)
    }
  }
  private def push (p: Int) {
    for (s <- h until 0 by -1) {
      val i = p >>> s
      if (d(i) != zero) {
        apply (i << 1, d(i))
        apply ((i << 1) + 1, d(i))
        d(i) = zero
      }
    }
  }
  def update (l: Int, r: Int, v: T) {
    @tailrec def loop (i: Int, j: Int) {
      if (i < j) {
        if (0 != (i & 1)) apply (i, v)
        if (0 != (j & 1)) apply (j - 1, v)
        loop ((i + 1) >>> 1, j >>> 1)
      }
    }
    val l0 = l + n
    val r0 = r + n
    loop (l0, r0)
    build (l0)
    build (r0 - 1)
  }
  def reduce (l:Int, r: Int): T = {
    val l0 = l + n
    val r0 = r + n
    push (l0)
    push (r0 - 1)
    var res = empty
    @tailrec def loop (i: Int, j: Int) {
      if (i < j) {
        if (0 != (i & 1)) res = max (res, t(i))
        if (0 != (j & 1)) res = max (t(j-1), res)
        loop ((i + 1) >>> 1, j  >>> 1)
      }
    }
    loop (l0, r0)
    res
  }
}
