const val modulo = 1_000_000_007
fun addm(x: Int, y: Int): Int {
  val z = x + y
  return if(z >= modulo) z - modulo else z
}

fun mulm(x: Int, y: Int): Int = ((x.toLong() * y) % modulo).toInt()
