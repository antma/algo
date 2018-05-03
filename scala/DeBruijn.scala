class DeBruijn {
  val db = 0x218A392CD3D5DBFL
  val t = Array.ofDim[Int] (64)
  for (k <- 0 to 63) t(((db << k) >>> 58).toInt) = k
  def lookup (x: Long) = t(((x * db) >>> 58).toInt)
}
