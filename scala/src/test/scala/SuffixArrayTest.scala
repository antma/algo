package com.github.antma.cpalgo
import org.specs2.mutable.Specification

class SuffixArrayTest extends Specification {
  "SuffixArray" should {
    "build" in {
      val text = "aba"
      val sa   = new SuffixArray(text)
      sa.o.mkString(",") must be equalTo "2,0,1"
    }
    "find" in {
      //          01234567890
      val text = "abacabadaba"
      val sa   = new SuffixArray(text)
      sa.o.mkString(",") must be equalTo "10,8,0,4,2,6,9,1,5,3,7"
      sa.find("daba") must be equalTo SASuffix(10)
      sa.find("d") must be equalTo SAInterval(9, 10)
      sa.find("e") must be equalTo SAInterval(10, 11)
      sa.find("0") must be equalTo SAInterval(-1, 0)
    }
  }
}
