object FenwickTree {
  class IterA (start: Int, n: Int) extends Iterator[Int] {
    var i = start
    override def hasNext: Boolean = i < n
    override def next(): Int = {
      val t = i
      i |= i + 1
      t
    }
  }
  class IterR (start: Int) extends Iterator[Int] {
    var i = start
    override def hasNext: Boolean = i >= 0
    override def next(): Int = {
      val t = i
      i = (i & (i + 1)) - 1
      t
    }
  }
}

class FenwickTree[T : reflect.ClassTag] (n: Int, op: (T, T) => T, default_value: T) {
  val a = Array.fill (n) (default_value)
  def add (x: Int, v: T) {
    for (i <- new FenwickTree.IterA (x, n)) {
      a(i) = op (a(i), v)  
    }
  }
  def reduce (x: Int): T = {
    var r = default_value
    for (i <- new FenwickTree.IterR (x)) {
      r = op (r, a(i))
    }
    r
  }
}
