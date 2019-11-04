import kotlin.math.*

//p^q (mod m)
fun powMod (p: Int, q: Int, m: Int): Int {
  var a = 1L
  var b = p.toLong()
  var y = q
  while (y > 0) {
    if ((y and 1) != 0) {
      a = (a * b) % m
    }
    b = (b * b) % m
    y = y ushr 1
  }
  return a.toInt();
}

fun sieveArray(n: Int): IntArray {
  val a = IntArray (n + 1) { if ((it and 1) == 1 || it == 0) it else 2 }
  for (p in 3..(sqrt(n.toDouble()) + 1e-9).toInt() step 2) {
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
