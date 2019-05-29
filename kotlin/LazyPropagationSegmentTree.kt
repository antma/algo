import java.io.*

typealias Value = Int
typealias ValueArray = IntArray
typealias Delta = Int

abstract class LazyPropagationSegmentTree(a: ValueArray) {
  val n = a.size
  val h = 31 - Integer.numberOfLeadingZeros(n)
  val t = ValueArray (2 * n) { if (it < n) 0 else a[it - n] }
  val d = Array<Delta?> (n) { null }
  abstract fun calc (p: Int, k: Int): Unit
  abstract fun apply (p: Int, delta: Delta, k: Int): Unit
  fun build (l0: Int, r0: Int) {
    var k = 2
    var l = l0 + n
    var r = r0 + n - 1
    while (l > 1) {
      l = l shr 1
      r = r shr 1
      for (i in r downTo l) {
        calc (i, k)
      }
      k *= 2
    }
  }
  fun push(l0: Int, r0: Int) {
    var s = h
    var k = 1 shl (h - 1)
    val l = l0 + n
    val r = r0 + n - 1
    while (s > 0) {
      for (i in (l shr s)..(r shr s)) {
        val delta = d[i]
        if (delta != null) {
          apply (i shl 1, delta, k)
          apply ((i shl 1) or 1, delta, k)
          d[i] = null
        }
      }
      s -= 1
      k = k shr 1
    }
  }
  init {
    build(0, n)
  }
}

class AssignSumLazyPropagationSegmentTree(a: ValueArray): LazyPropagationSegmentTree(a) {
  override fun calc (p: Int, k: Int): Unit {
    val dp = d[p]
    t[p] = if (dp == null) t[p shl 1] + t[(p shl 1) or 1] else dp * k
  }

  override fun apply (p: Int, delta: Delta, k: Int): Unit {
    t[p] = delta * k
    if (p < n) {
      d[p] = delta
    }
  }

  fun assign (l0: Int, r0: Int, value: Value) {
    push (l0, l0 + 1)
    push (r0 - 1, r0)
    var l = l0 + n
    var r = r0 + n
    var k = 1
    while (l < r) {
      if ((l and 1) != 0) {
        apply (l, value, k)
        l += 1
      }
      if ((r and 1) != 0) {
        r -= 1
        apply (r, value, k)
      }
      l = l shr 1
      r = r shr 1
      k *= 2
    }
    build (l0, l0 + 1)
    build (r0 - 1, r0)
  }

  fun sum (l0: Int, r0: Int):Value {
    push (l0, l0 + 1)
    push (r0 - 1, r0)
    var res:Value = 0
    var l = l0 + n
    var r = r0 + n
    while (l < r) {
      if ((l and 1) != 0) {
        res += t[l]
        l += 1
      }
      if ((r and 1) != 0) {
        r -= 1
        res += t[r]
      }
      l = l shr 1
      r = r shr 1
    }
    return res
  }
}
