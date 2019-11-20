package cpalgo.fastio
import java.io.*

class FastScanner(private val input: InputStream) {
  companion object {
    val zero = 0.toByte()
    val cr = '\r'.toByte()
    val nl = '\n'.toByte()
    val BUFFER_SIZE = 0x10000
  }
  private val b = ByteArray(BUFFER_SIZE)
  private var n = 0
  private var pos = 0
  private var eof = false
  fun read ():Boolean {
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
      sb.append(b.toChar())
    }
  }
  fun nextByte(): Byte {
    if (pos >= n && !read ()) {
      return 0
    }
    val r = b[pos]
    ++pos
    return r
  }
  fun nextInt():Int {
    var b:Byte
    do {
      b = nextByte ()
    } while (b <= 32)
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
}
