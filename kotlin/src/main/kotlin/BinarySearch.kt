fun IntArray.lowerBound(x: Int): Int {
  var l = -1
  var r = size
  while (r - l > 1) {
    val m = (l + r) shr 1
    if(get(m) < x) l = m else r = m
  }
  return r
}
