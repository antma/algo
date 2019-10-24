import java.io.*

fun parseUnsignedInt(s:String, i: Int, j: Int): Int {
  var x = 0
  for (k in i until j) {
    val u = s[k].toInt () - 48
    assert (u >= 0 && u < 10)
    x = 10 * x + u
  }
  return x
}

fun parseInt(s:String, i: Int, j: Int): Int {
  if (s[i] == '-') {
    var x = 0
    for (k in (i + 1) until j) {
      val u = s[k].toInt () - 48
      assert (u >= 0 && u < 10)
      x = 10 * x - u
    }
    return x
  } else {
    var x = 0
    for (k in i until j) {
      val u = s[k].toInt () - 48
      assert (u >= 0 && u < 10)
      x = 10 * x + u
    }
    return x
  }
}

fun parseUnsignedLong(s:String, i: Int, j: Int): Long {
  var x = 0L
  for (k in i until j) {
    val u = s[k].toInt () - 48
    assert (u >= 0 && u < 10)
    x = 10 * x + u
  }
  return x
}

fun parseLong(s:String, i: Int, j: Int): Long {
  if (s[i] == '-') {
    var x = 0L
    for (k in (i + 1) until j) {
      val u = s[k].toInt () - 48
      assert (u >= 0 && u < 10)
      x = 10 * x - u
    }
    return x
  } else {
    var x = 0L
    for (k in i until j) {
      val u = s[k].toInt () - 48
      assert (u >= 0 && u < 10)
      x = 10 * x + u
    }
    return x
  }
}

class FastScanner() {
  val linesIt = BufferedReader(InputStreamReader(System.`in`), 0x100000)
    .lineSequence()
    .iterator()
  val re = Regex("\\s+")
  var pos = 0
  var s = ""
  inline fun <T> next(parse: (String, Int, Int) -> T):T {
    while (true) {
      while (pos >= s.length) {
        s = linesIt.next ()
        pos = 0
      }
      val m = re.find (s, pos)
      if (m != null) {
        val r = m.range
        val e = r.endInclusive + 1
        if (pos == r.start) {
          pos = e
        } else {
          val res = parse (s, pos, r.start)
          pos = e
          return res
        }
      } else {
        val res = parse (s, pos, s.length)
        pos = Int.MAX_VALUE
        return res
      }
    }
  }
}
