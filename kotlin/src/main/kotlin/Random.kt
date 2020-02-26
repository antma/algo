import kotlin.random.Random

fun IntArray.shuffle(r: Random) {
  val n = size
  for (i in 1 until n) {
    val j = i + r.nextInt(n - i)
    if (i < j) {
      val t = this[i]
      this[i] = this[j]
      this[j] = t
    }
  }
}
