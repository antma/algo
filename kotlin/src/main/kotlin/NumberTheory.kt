package cpalgo.numtheory
import kotlin.math.*
import java.util.BitSet

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

class LinearSieve(val n: Int, prime: (Int) -> Int, divides : (Int, Int) -> Int) {
  private val composite = BitSet(n)
  private val primes = arrayListOf<Int>()
  private val f = IntArray(n)
  operator fun get(i: Int) = f[i]
  init {
    f[1] = 1
    for (i in 2 until n) {
      if (!composite[i]) {
        primes.add(i)
        f[i] = prime(i)
      }
      for (j in primes) {
        val k = i * j
        if (k >= n) {
          break
        }
        composite[k] = true
        if (i % j == 0) {
          f[k] = divides(f[i], j)
          break
        } else {
          f[k] = f[i] * f[j]
        }
      }
    }
  }
}
