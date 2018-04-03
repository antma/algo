import scala.reflect.ClassTag
class SegmentTreeSliceUpdate[T : ClassTag] (_a: Array[T], op: (T, T) => T) {
  val a = _a
  val n = a.length
  val t = Array.ofDim[T] (4 * n)
  private def build (v: Int, l: Int, r: Int): Unit = {
    if (l == r) {
      t(v) = a(l)
    } else {
      val m = (l + r) >> 1
      build (v << 1, l, m)
      build ((v << 1) + 1, m + 1, r)
    }
  }
  build (1, 0, n - 1)
  private def update (v: Int, l: Int, r: Int, a: Int, b: Int, value: T) : Unit = {
    if (a <= b) {
      if (l == a && r == b) {
        t(v) = op (t(v), value)
      } else {
        val m = (l + r) >> 1
        val w = (v << 1)
        update (w, l, m, a, m.min (b), value)
        update (w + 1, m + 1, r, (m+1).max (a), b, value)
      }
    }
  }
  private def get (v: Int, l: Int, r: Int, a: Int): T = {
    if (l == r) t (v)
    else {
      val m = (l + r) >> 1
      op (t(v), if (a <= m) get ((v << 1), l, m, a) else get ((v << 1) + 1, m + 1, r, a))
    }
  }
  def update (a: Int, b: Int, value: T): Unit = {
    update (1, 0, n - 1, a, b, value)
  }
  def apply (a: Int) = get (1, 0, n - 1, a)
}
