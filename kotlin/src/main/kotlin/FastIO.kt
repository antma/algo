package cpalgo.fastio
import java.io.*

class FastScanner(private val input: InputStream, buffer_size: Int) {
  companion object {
    val zero = 0.toByte()
    val cr = 0xd.toByte()
    val nl = 0xa.toByte()
  }
  private val b = ByteArray(buffer_size)
  private var n = 0
  private var pos = 0
  private var eof = false
  private fun read ():Boolean {
    if (eof) {
      return false
    }
    pos = 0
    n = input.read(b)
    if (n < 0) {
      eof = true
    }
    return n > 0
  }
  private fun nextByte(): Byte {
    if (pos >= n && !read ()) {
      return 0
    }
    val r = b[pos]
    ++pos
    return r
  }
  private fun skipBlanks(): Byte {
    while(true) {
      val b = nextByte()
      if(b > 32 || b < 1) return b
    }
  }
  fun nextLine(k: Int = -1): String? {
    val sb = if(k >= 0) StringBuilder(k) else StringBuilder()
    while(true) {
      val b = nextByte()
      if (b == zero) {
        return if (sb.length == 0) null else sb.toString()
      }
      if (b == nl) {
        return sb.toString()
      }
      if (b == cr) {
        check (nextByte() == nl)
        return sb.toString()
      }
      sb.append(b.toInt().toChar())
    }
  }
  fun nextToken(k: Int = -1): String {
    val sb = if(k >= 0) StringBuilder(k) else StringBuilder()
    var b = skipBlanks()
    check(b > 0)
    while(b > 32) {
      sb.append(b.toInt().toChar())
      b = nextByte()
    }
    return sb.toString()
  }
  fun nextInt():Int {
    var b = skipBlanks()
    check(b > 0)
    if (b < 48) {
      var t = 0
      while (true) {
        b = nextByte ()
        if (b <= 32) {
          return t
        }
        t = 10 * t - (b - 48)
      }
    } else {
      var t = b - 48
      while (true) {
        b = nextByte ()
        if (b <= 32) {
          return t
        }
        t = 10 * t + (b - 48)
      }
    }
  }
  fun nextLong():Long {
    var b = skipBlanks()
    check(b > 0)
    if (b < 48) {
      var t = 0L
      while (true) {
        b = nextByte ()
        if (b <= 32) {
          return t
        }
        t = 10 * t - (b - 48)
      }
    } else {
      var t = (b - 48).toLong()
      while (true) {
        b = nextByte ()
        if (b <= 32) {
          return t
        }
        t = 10 * t + (b - 48)
      }
    }
  }
}

object FastIO {
  val BUFFER_SIZE = 0x100000
  fun scanner(): FastScanner =
    FastScanner(System.`in`, BUFFER_SIZE)
  fun writer(): PrintWriter =
    PrintWriter(BufferedWriter(OutputStreamWriter(System.out), BUFFER_SIZE), false)
}
