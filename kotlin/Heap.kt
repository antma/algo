typealias Value = Int
typealias ValueArray = IntArray

class Heap (private val n: Int, default: Value) {
  private var size = 0
  private val a = ValueArray (n, { default } )
  private val h = IntArray (n + 1)
  private val g = IntArray (n)
  private fun heapifyFront (k: Int) {
    val he = h[k]
    var i = k
    var j = 2 * i
    while (j <= size) {
      if (j < size && a[h[j+1]] < a[h[j]]) {
        j += 1
      }
      if (a[h[j]] >= a[he]) {
        break
      }
      h[i] = h[j]
      g[h[i]] = i
      i = j
      j = 2 * i
    }
    if (i != k) {
      h[i] = he
      g[he] = i
    }
  }
  private fun heapifyBack (k: Int) {
    val he = h[k]
    var i = k
    while (i > 1) {
      val j = i shr 1
      if (a[he] >= a[h[j]]) {
        break
      }
      h[i] = h[j]
      g[h[i]] = i
      i = j
    }
    if (i != k) {
      h[i] = he
      g[he] = i
    }
  }
  private fun insert (i: Int) {
    size += 1
    h[size] = i
    g[i] = size
    heapifyBack (size)
  }
  fun decreaseKey (k: Int, value: Value) {
    assert (k >= 0 && k < n)
    assert (value < a[k])
    a[k] = value
    val pos = g[k]
    if (pos == 0) {
      insert (k)
    } else {
      heapifyBack (pos)
    }
  }
  fun extractMin (): Int {
    assert (size > 0)
    val he = h[1]
    g[he] = 0
    size -= 1
    if (size > 0) {
      h[1] = h[size+1]
      g[h[1]] = 1
      heapifyFront (1)
    }
    return he
  }
  fun isEmpty() = size == 0
  operator fun get(index: Int): Value = a[index]
}
