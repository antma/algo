class SegmentTree[T : reflect.ClassTag] (_a: Array[T], op: (T, T) => T) {
  val a = _a
  val n = a.length
  val t = Array.ofDim (4 * n)
  private def build (v: Int, l: Int, r: Int): Unit = {
    if (l == r) {
      t(v) = a(l)
    } else {
      val m = (l + r) >> 1
      build (v << 1, l, m)
      build ((v << 1) + 1, m + 1, r)
      t(v) = op (t(v << 1), t((v << 1) + 1))
    }
  }
  build (1, 0, n - 1)
  private def reduce (v: Int, l: Int, r: Int, a: Int, b: Int): T =  {
    if (a == l && b == r) {
      t(v)
    } else {
      val m = (l + r) >> 1
      val x = b.min (m)
      val y = a.max (m + 1)
      val w = 2 * v
      if (a <= x) {
        if (y <= b) {
          op (reduce (w, l, m, a, x), reduce (w + 1, m + 1, r, y, b))
        } else {
          reduce (w, l, m, a, x)
        }
      } else {
        reduce (w + 1, m + 1, r, y, b)
      }
    }
  }
  def reduce (a: Int, b: Int): T = reduce (1, 0, n - 1, a, b)
  def update (i: Int, new_value: T) = {
    var l = 0
    var r = n - 1
    var v = 1
    while (l < r) {
      val m = (l + r) >> 1
      v <<= 1
      if (i <= m) {
        r = m
      } else {
        v += 1
        l = m + 1
      }
    }
    t(v) = new_value
    while (v > 1) {
      v &= ~1
      t(v >> 1) = op (t(v), t(v + 1))
      v >>= 1
    }
  }
}

class SegmentTree2D[T : ClassTag] (_a: Array[T], build_op: (T, T) => T) extends SegmentTree[T] (_a, build_op) {
  private def reduce2d[U] (v: Int, l: Int, r: Int, a: Int, b: Int, extract_op: (T) => U, reduce_op: (U, U) => U): U =  {
    if (a == l && b == r) {
      extract_op (t(v))
    } else {
      val m = (l + r) >> 1
      val x = b.min (m)
      val y = a.max (m + 1)
      val w = 2 * v
      if (a <= x) {
        if (y <= b) {
          reduce_op (reduce2d (w, l, m, a, x, extract_op, reduce_op), reduce2d (w + 1, m + 1, r, y, b, extract_op, reduce_op))
        } else {
          reduce2d (w, l, m, a, x, extract_op, reduce_op)
        }
      } else {
        reduce2d (w + 1, m + 1, r, y, b, extract_op, reduce_op)
      }
    }
  }
  def reduce2d[U] (a: Int, b: Int, extract_op: (T) => U, reduce_op: (U, U) => U): U = reduce2d (1, 0, n - 1, a, b, extract_op, reduce_op)
}
