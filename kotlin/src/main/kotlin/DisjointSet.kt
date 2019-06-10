class DisjointSet(n: Int) {
  val p = IntArray(n) { it }
  val h = IntArray(n)
  fun findSet(x: Int): Int {
    if (p[x] == x) return x
    else {
      val r = findSet(p[x])
      p[x] = r
      return r
    }
  }
  fun merge (x: Int, y: Int): Boolean {
    val i = findSet(x)
    val j = findSet(y)
    return when {
      i == j -> false
      h[i] < h[j] -> { p[i] = j; true }
      h[i] > h[j] -> { p[j] = i; true }
      else -> { p[i] = j; h[j] += 1; true }
    }
  }
}
