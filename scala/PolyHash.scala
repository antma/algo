object PolyHash {
  private val primes = Array (2147483647, 2147483629, 2147483587, 2147483579, 2147483563, 2147483549, 2147483543, 2147483497, 2147483489, 2147483477, 2147483423, 2147483399, 2147483353, 2147483323, 2147483269, 2147483249, 2147483237, 2147483179, 2147483171, 2147483137)
  def randomP () = {
    val r = new util.Random (System.nanoTime ())
    primes (r.nextInt (primes.size))
  }
}

class PolyHash (s: String, p: Int) {
  val n = s.length
  val h = s.scanLeft (0L) ((v, c) => v * p + c.toInt)
  val d = Array.iterate (1L, n + 1) (v => v * p)
  def get (l: Int, r: Int) = h(r) - h(l) * d(r - l)
}
