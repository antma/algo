package cpalgo.queue

class IntQueue(bits: Int) {
  val mask = (1 shl bits) - 1
  val q = IntArray(mask + 1)
  var left = 0
  var right = 0
  fun clear() {
    left = 0
    right = 0
  }
  fun add(x: Int) {
    check((((right xor left) and mask) != 0) || left == right){"queue overflow"}
    q[(right++) and mask] = x
  }
  fun isEmpty() = left == right
  fun poll(): Int {
    return q[(left++) and mask]
  }
}

fun queueBits(n: Int): Int = 32 - Integer.numberOfLeadingZeros(n)
