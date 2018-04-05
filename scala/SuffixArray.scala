import scala.collection.SeqView

class SuffixArray (_s: String) {
  val s = _s :+ 0.toChar
  val n = _s.length
  val alphabet_size = (_s.fold (0.toChar) ( (x, y) => x.max (y))).toInt + 1

  private def counting_sort (m: Int, c: Array[Int], p: Seq[Int], o: Array[Int]): Unit = {
    val cnt:Array[Int] = Array.ofDim (m)
    p.foreach (pi => cnt(c(pi)) = cnt(c(pi)) + 1)
    (1 until m).foreach (i => cnt(i) = cnt(i) + cnt(i-1))
    p.reverseIterator.foreach (pi => {
      cnt(c(pi)) = cnt(c(pi)) - 1
      o(cnt(c(pi))) = pi
    })
  }
  private def build ():Array[Int] = {
    val l = s.length
    var p: Array[Int] = Array.ofDim (l)
    var c = s.map (c => c.toInt).toArray
    var q: Array[Int] = Array.ofDim (l)
    counting_sort (alphabet_size, c, (0 until l), p)
    c(p(0)) = 0
    var m = 0
    for (i <- 1 until l) {
      if (s(p(i)) != s(p(i-1))) {
        m = m + 1
      }
      c(p(i)) = m
    }
    m = m + 1
    var step = 1
    while (step < l) {
      counting_sort (m, c, p.map (v => (v + l - step) % l), q)
      val t1 = p; p = q; q = t1
      q(p(0)) = 0
      m = 0
      for (i <- 1 until l) {
        if (c(p(i)) != c(p(i-1)) || c((p(i) + step) % l) != c((p(i-1) + step) % l)) {
          m = m + 1
        }
        q(p(i)) = m
      }
      m = m + 1
      val t2 = c; c = q; q = t2
      step = 2 * step
    }
    p.slice (1, l)
  }
  val o = build
  private def lcp_table (a: Array[Int], l: Int, r: Int): Int = {
    if (r - l == 1) {
      a(r)
    } else {
      val m = (l + r) >> 1
      val res = Math.min (lcp_table (a, l, m), lcp_table (a, m, r))
      a(n + 1 + m) = res
      res
    }
  }
  private def lcp_build () = {
    var q: Array[Int] = Array.ofDim (2 * n + 1)
    val r: Array[Int] = Array.ofDim (n)
    (0 until n).foreach (i => r(o(i)) = i)
    var l = 0
    for (j <- 0 until n) {
      l = 0.max (l - 1)
      val i = r(j)
      if (i > 0) {
        val k = o(i - 1)
        while ((j + l < n) && (k + l < n) && s(j + l) == s(k + l)) {
          l = l + 1
        }
      } else {
        l = 0
      }
      q(i) = l
    }
    val t = lcp_table (q, -1, n)
    q
  }
  val lcp = lcp_build
  def find (x: SeqView[Char, String]) : (Int, Int) = {
    def l (l: Int, r: Int): Int = if (r - l == 1) lcp (r) else lcp (n + 1 + ((l + r) >> 1))
    def cl (u: Int, l: Int): Int = {
      if (u < 0 || u >= n) 0
      else {
        val s1 = x.slice (l, x.length)
        val s2 = s.view (o (u) + l, n)
        l + s1.zip (s2).takeWhile ( t => t._1 == t._2).length
      }
    }
    def f (u: Int, lu: Int, v: Int, lv: Int): (Int, Int) = {
      if (u + 1 >= v) {
       // (-1, Math.max (cl (u, lu), cl (v, lv)))
        (-1, Math.max (lu, lv))
      } else {
        val m = (u + v) >> 1

        if (lu <= l (m, v) && l (m, v) < lv) {
          f (m, l (m, v), v, lv)
        } else if (lu <= lv && lv < l (m, v)) {
          f (u, lu, m, lv)
        } else if (lv <= l (u, m) && l (u, m) < lu) {
          f (u, lu, m, l (u, m))
        } else if (lv <= lu && lu < l (u, m)) {
          f (m, lu, v, lv)
        } else {
          val k = Math.max (lu, lv)
          val s1 = x.slice (k, x.length)
          val lm = n - o (m)
          val s2 = s.view (o(m) + k, n)
          val l = k + s1.zip (s2).takeWhile ( t => t._1 == t._2).length
          if (l == x.length && l == lm) {
            (m, l)
          } else if ((l == lm) || (l != x.length && s(o(m)+l) < x(l))) {
            f (m, l, v, lv)
          } else {
            f (u, lu, m, l)
          }
        }
      }
    }
    f (-1, 0, n, 0)
  }
}
