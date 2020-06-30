package org.github.antma.cpalgo
import org.specs2.mutable.Specification

class FastIOTest extends Specification {
  def fromString(s: String) = new FastScanner(new java.io.ByteArrayInputStream(s.toCharArray.map { _.toByte }))
  "FastIO" should {
     "FastScanner.readInt()" in {
        val r = fromString("1 -10\r\n 2")
        r.nextInt() must be equalTo 1
        r.nextInt() must be equalTo -10
        r.nextInt() must be equalTo 2
        val f = try {
          Some(r.nextInt())
        } catch {
          case _ : IllegalArgumentException => None
        }
        f must beNone
     }
     "FastScanner.readDouble()" in {
        val r = fromString("123.456\n-2")
        r.nextDouble() must beCloseTo(123.456, 1e-10)
        r.nextDouble() must be equalTo -2.0
     }
     "FastScanner.readLong()" in {
        val r = fromString("-1234567890")
        r.nextLong() must be equalTo -1234567890L
     }
  }
}
