package org.github.antma.cpalgo
//Origin: http://codeforces.com/blog/entry/18051

import annotation.tailrec

class SegmentTree [T: reflect.ClassTag] (a: Array[T], op: (T, T) => T, empty: T) {
  private val n = a.size
  private val t = Array.ofDim (2*n)
  private def build(): Unit = {
    for (i <- n - 1 until 0 by - 1) {
      val k = i << 1
      t(i) = op (t(k), t(k+1))
    }
  }
  Array.copy (a, 0, t, n, n)
  build()
  final def update (p: Int, v: T) = {
    val p0 = p + n
    t(p0) = v
    @tailrec def loop (i: Int): Unit = {
      if (i > 1) {
        val k = i >>> 1
        t(k) = op (t(i), t(i^1))
        loop (k)
      }
    }
    loop (p0)
  }
  //op - commutative
  final def reduce (l: Int, r: Int): T = {
    var res = empty
    @tailrec def loop (i: Int, j: Int): Unit = {
      if (i < j) {
        if (0 != (i & 1)) res = op (res, t(i))
        if (0 != (j & 1)) res = op (t(j-1), res)
        loop ((i + 1) >>> 1, j  >>> 1)
      }
    }
    loop (l + n, r + n)
    res
  }
}

//Modification on semi-interval, single element access
class SegmentTreeSliceUpdate [T: reflect.ClassTag] (n: Int, add: (T, T) => T, zero: T) {
  private val t = Array.fill (2 * n)(zero)
  private def push() = {
    for (i <- 1 until n) {
      val k = i << 1
      val v = t(i)
      t(k) = add (t(k), v)
      t(k+1) = add (t(k+1), v)
      t(i) = zero
    }
  }
  final def update (l: Int, r: Int, v: T) = {
    val l0 = l + n
    val r0 = r + n
    @tailrec def loop (i: Int, j: Int): Unit = {
      if (i < j) {
        if (0 != (i & 1)) t(i) = add (t(i), v)
        if (0 != (j & 1)) t(j-1) = add (t(j-1), v)
        loop ((i + 1) >>> 1, j  >>> 1)
      }
    }
    loop (l0, r0)
  }
  final def apply (p: Int): T = {
    var res = zero
    var k = p + n
    while (k > 0) {
      res = add (res, t(k))
      k >>= 1
    }
    res
  }
  final def force() = {
    push()
    t.slice (n, 2 * n)
  }
}

//commutative lazy propagation segment tree (increment modifications, queries on mininum/maximum)
class SegmentTreeCommutativeLazyPropagation [T: reflect.ClassTag] (n: Int, add: (T, T) => T, max: (T, T) => T, empty: T, zero: T) {
  private val t = Array.fill (2 * n)(zero)
  private val d = Array.fill (n)(zero)
  private val h = (0 to 30).find (i => n < (1 << i)).head
  private def apply (p: Int, v: T) = {
    t(p) = add (t(p), v)
    if (p < n) d(p) = add (d(p), v)
  }
  @tailrec private def build (p: Int): Unit = {
    if (p > 1) {
      val q = p >>> 1
      t(q) = add (max (t(q<<1), t((q<<1) + 1)), d(q))
      build (q)
    }
  }
  private def push (p: Int) = {
    for (s <- h until 0 by -1) {
      val i = p >>> s
      if (d(i) != zero) {
        apply (i << 1, d(i))
        apply ((i << 1) + 1, d(i))
        d(i) = zero
      }
    }
  }
  def update (l: Int, r: Int, v: T): Unit = {
    @tailrec def loop (i: Int, j: Int): Unit = {
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
    @tailrec def loop (i: Int, j: Int): Unit = {
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

//T - Set (found maximum in rectangle for constant set of Points)
class SegmentTreeSet [T: reflect.ClassTag] (a: Array[T], combine: (T, T) => T) {
  private val n = a.size
  private val t = Array.ofDim (2*n)
  private def build(): Unit = {
    for (i <- n - 1 until 0 by - 1) {
      val k = i << 1
      t(i) = combine (t(k), t(k+1))
    }
  }
  Array.copy (a, 0, t, n, n)
  build()
  //op - commutative
  def reduce[U] (l: Int, r: Int, extract: (T) => U, op: (U, U) => U, empty: U) = {
    var res = empty
    @tailrec def loop (i: Int, j: Int): Unit = {
      if (i < j) {
        if (0 != (i & 1)) res = op (res, extract (t(i)))
        if (0 != (j & 1)) res = op (extract (t(j-1)), res)
        loop ((i + 1) >>> 1, j  >>> 1)
      }
    }
    loop (l + n, r + n)
    res
  }
}
