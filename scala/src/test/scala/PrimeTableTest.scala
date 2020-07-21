package com.github.antma.cpalgo
import org.specs2.mutable.Specification

class PrimeTableTest extends Specification {
  "PrimeTable" should {
    "sum of primes <1000000" in {
      new PrimeTable(1000000).primes.foldRight(0L) { _ + _ } must_== 37550402023L
    }
    "primes below 200 tests" in {
      val p = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89,
        97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193,
        197, 199)
      (1 until 200).forall { n =>
        new PrimeTable(n).primes.sameElements(p.takeWhile { _ < n })
      } must beTrue
    }
  }
}
