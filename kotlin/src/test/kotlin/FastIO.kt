import kotlin.test.assertEquals
import kotlin.test.assertTrue
import java.io.ByteArrayInputStream
import org.junit.Test

import cpalgo.fastio.*

class FastIO {
  @Test
  fun testReadLine() {
    val b = byteArrayOf(0x61, 0x62, 0x61, 0x63, 0x61, 0x62, 0x61, 0x0a, 
      0x63, 0x61, 0x74, 0x0d, 0x0a,
      0x64, 0x6f, 0x67, 0x0a)
    val f = FastScanner(ByteArrayInputStream(b), 0x100)
    assertEquals(f.nextLine(7)!!, "abacaba")
    assertEquals(f.nextLine()!!, "cat")
    assertEquals(f.nextLine()!!, "dog")
    assertTrue(f.nextLine() == null)
  }
  @Test
  fun testNextInt() {
    val b = byteArrayOf(0x31, 0x32, 0x33, 0x34, 0x35, 0x20, 0x2d, 0x32, 0x0a)
    val f = FastScanner(ByteArrayInputStream(b), 0x100)
    assertEquals(f.nextInt(), 12345)
    assertEquals(f.nextInt(), -2)
  }
}
