import kotlin.test.assertEquals
import kotlin.test.assertTrue
import org.junit.Test

import cpalgo.primes.*

val largePrimes = primes(1000000)

class PrimeTableTest {
  @Test
  fun testPrimesLarge() {
    assertEquals(largePrimes.map { it.toLong() }.sum(), 37550402023L)
  }
  @Test
  fun testPrimesSmall() {
    for (i in 1 .. 100) {
      val l1 = largePrimes.takeWhile { it < i }.toIntArray ()
      val l2 = primes(i)
      assertTrue (l1.contentEquals(l2))
    }
  }
}
