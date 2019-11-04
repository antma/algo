import kotlin.test.assertTrue
import kotlin.test.assertFailsWith
import org.junit.Test

import cpalgo.queue.*

class QueueTest {
  @Test
  fun testPrimesLarge() {
    val q = IntQueue(2)
    assertTrue(q.isEmpty())
    for (i in 1 .. 4) {
      q.add(i)
    }
    for (i in 1 .. 4) {
      assertTrue(q.poll() == i)
    }
    assertTrue(q.isEmpty())
    for (i in 1 .. 4) {
      q.add(i)
    }
    assertFailsWith (IllegalStateException::class) { q.add(5) }
  }
}
