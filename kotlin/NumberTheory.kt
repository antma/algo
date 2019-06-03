import kotlin.math.*

fun sieveArray(n: Int): IntArray {
  val a = IntArray (n + 1) { if ((it and 1) == 1 || it == 0) it else 2 }
  for (p in 3..sqrt(n.toDouble()).toInt() step 2) {
    if (a[p] == p) {
      for (o in p * p..n step 2 * p) {
        if (a[o] == o) {
          a[o] = p
        }
      }
    }
  }
  return a
}
