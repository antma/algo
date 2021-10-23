const val modulo = 1_000_000_007
fun addm(x: Int, y: Int): Int {
  val z = x + y
  return if(z >= modulo) z - modulo else z
}

fun subm(x: Int, y: Int): Int {
  val z = x - y
  return if (z < 0) z + modulo else z
}

fun mulm(x: Int, y: Int): Int = ((x.toLong() * y) % modulo).toInt()

fun gcdext(a: Int, b: Int): Pair<Int, Int> {
  if(b == 0) return Pair(1, 0)
  val t = a / b
  val (y, x) = gcdext(b, a - t * b)
  return Pair(x, y - x * t)
}

fun powm(x: Int, p: Int): Int {
  if(p == 0) return 1
  var b = x
  var y = p
  while(0 == y and 1) {
    b = mulm(b, b)
    y = y ushr 1
  }
  var a = b
  y = y ushr 1
  while(y != 0) {
    b = mulm(b, b)
    if(0 != y and 1) {
      a = mulm(a, b)
    }
    y = y ushr 1
  }
  return a
}

fun inversion(v: Int): Int {
  val (_, x) = gcdext (modulo, v)
  val r = if (x < 0) x + modulo else x
  return r
}
