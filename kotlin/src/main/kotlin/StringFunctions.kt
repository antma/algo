import kotlin.math.min

fun computePrefixFunction(s: String): IntArray {
  val n = s.length
  val p = IntArray(n)
  var k = 0
  for (q in 1 until n) {
    while (k > 0 && s[k] != s[q]) {
      k = p[k-1]
    }
    if (s[k] == s[q]) {
      ++k
    }
    p[q] = k
  }
  return p
}

fun computeZFunction(s: String): IntArray {
  val n = s.length
  val z = IntArray(n)
  var l = 0
  var r = 0
  for (i in 1 until n) {
    var k = if (i <= r) min (r - i + 1, z[i - l]) else 0
    while (i + k < n && s[k] == s[i+k]) {
      ++k
    }
    val j = i + k - 1
    if (j > r) {
      l = i
      r = j
    }
    z[i] = k
  }
  return z
}
