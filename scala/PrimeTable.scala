import scala.collection.mutable.BitSet

class PrimeTable (n: Int) { //finding primes < n
  val half = n / 2
  val m = Math.floor (Math.sqrt (n) * 0.5).toInt
  def f (b: BitSet, i: Int): BitSet = {
    if (b.contains (i)) b
    else {
      val step = 2 * i + 1
      val start = 2 * i * (i + 1)
      val k = half / step
      b ++= (start to (start + k * step) by step)
    }
  }
  //c.contains (k) if 2 * k + 1 is composite number
  val c = (1 to m).foldLeft (BitSet.empty + 0) (f)
  def primes () = if (n <= 2) List.empty else 2 +: (1 until half).filterNot (i => c.contains (i)).map (i => 2 * i + 1).toList
}
