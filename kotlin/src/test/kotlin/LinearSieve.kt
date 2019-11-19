import kotlin.test.assertEquals
import org.junit.Test

import cpalgo.numtheory.*

class LinearSieve {
  @Test
  fun testTotient() {
    val phi = intArrayOf(1, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4, 12, 6, 8, 8, 16, 6, 18, 8, 12, 10, 22, 8, 20, 12, 18, 12, 28, 8, 30, 16, 20, 16, 24, 12)
    val t = LinearSieve(phi.size + 1, { p -> p - 1 }, { acc, p -> acc * p})
    for (i in phi.indices) {
      assertEquals (t[i+1], phi[i])
    }
  }
}
