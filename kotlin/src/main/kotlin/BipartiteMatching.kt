fun bipartiteMatching(a: Array<IntArray>, n: Int, m: Int): Int {
  val boys = IntArray(n) { -1 }
  val girls = IntArray(m) { -1 }
  val used = BooleanArray(n)
  fun go(i: Int): Boolean {
    if (used[i]) {
      return false
    } else {
      used[i] = true
      for (j in a[i]) {
        if (girls[j] < 0 || go(girls[j])) {
          boys[i] = j
          girls[j] = i
          return true
        }
      }
      return false
    }
  }
  var res = 0
  for (i in 0 until n) {
    used.fill(false)
    if (go(i)) {
      res += 1
    }
  }
  var stop = false
  while (!stop) {
    stop = true
    used.fill(false)
    for (i in 0 until n) {
      if (boys[i] < 0 && go(i)) {
        res += 1
        stop = false
      }
    }
  }
  return res
}
