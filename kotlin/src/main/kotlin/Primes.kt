package cpalgo.primes

import java.util.BitSet
import kotlin.math.*

fun primes(n: Int) = PrimeTable(n).primes()

public class PrimeTable(val n: Int) {
  private val m = max(1, n / 2)
  val a = BitSet(m)
  init {
    a[0] = true
    for (i in 1..ceil(sqrt(n.toDouble())).toInt()) {
      if (!a[i]) {
        val k = 2 * i + 1
        for (j in 2 * i * (i + 1) until m step k) {
          a[j] = true
        }
      }
    }
    a.flip(0, m)
  }
  fun isPrime (i: Int): Boolean = if ((i and 1) != 0) a[i ushr 1] else i == 2
  fun primes(): IntArray {
    if (n < 3) {
      return IntArray(0)
    }
    val l = IntArray(a.cardinality()+1)
    var k = 0
    l[0] = 2
    for (i in a.stream()) {
      k++
      l[k] = 2 * i + 1
    }
    return l
  }
}
