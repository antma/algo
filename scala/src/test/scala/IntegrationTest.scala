package org.github.antma.cpalgo
import org.specs2.mutable.Specification
import scala.math.{sin, cos}

class IntegrationTest extends Specification {
  "Integration" should {
     "Integration.simpson()" in {
        Integration.simpson(0, 1, 1e-6, {x => x}) must beCloseTo(0.5, 1e-6)
        Integration.simpson(0, 1, 1e-6, {x => x * x}) must beCloseTo(1.0 / 3.0, 1e-6)
        Integration.simpson(-1, 1, 1e-6, {x => sin(x)}) must beCloseTo(0.0, 1e-6)
        Integration.simpson(-1, 1, 1e-6, {x => cos(x)}) must beCloseTo(2.0 * sin(1.0), 1e-5)
     }
  }
}
