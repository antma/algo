import kotlin.math.min

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
