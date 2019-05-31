object DeBruijnLong {
  const val db = 0x218A392CD3D5DBFL
  private val t = IntArray(64)
  init {
    for (k in 0 until 64) {
      t[((db shl k) ushr 58).toInt()] = k
    }
  }
  fun lookup(x: Long) = t[((x * db) ushr 58).toInt()]
}

class BitIteratorLong(l: Long) : Iterator<Int> {
  private var i = l
  override operator fun hasNext() = i != 0L
  override operator fun next(): Int {
    val u = i and (i - 1)
    val r = DeBruijnLong.lookup(i xor u)
    i = u
    return r
  }
}
