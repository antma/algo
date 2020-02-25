const val modulo = 1_000_000_007
fun addm(x: Int, y: Int): Int {
  val z = x + y
  return if(z >= modulo) z - modulo else z
}

fun mulm(x: Int, y: Int): Int = ((x.toLong() * y) % modulo).toInt()

fun gcdext(a: Int, b: Int): Pair<Int, Int> {
  if(b == 0) return Pair(1, 0)
  val t = a / b
  val (y, x) = gcdext(b, a - t * b)
  return Pair(x, y - x * t)
}

fun inversion(v: Int): Int {
  val (_, x) = gcdext (modulo, v)
  val r = if (x < 0) x + modulo else x
  return r
}
