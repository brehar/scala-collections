package mathlib

import org.scalacheck.Test
import org.scalacheck.Test.{ Parameters, Result, TestCallback }
import org.scalacheck.util.Pretty

trait AlgebraicStructure[A] {
  AlgebraicStructure.ensureLawsHold(laws)

  def laws: Set[Law]
}

object AlgebraicStructure {
  private def ensureLawsHold(laws: Set[Law]): Unit = Test.check(parameters, laws.reduce(_ && _))

  private val parameters: Parameters = Parameters.default.withTestCallback {
    new TestCallback {
      final override def onTestResult(name: String, result: Result): Unit =
        if (!result.passed) sys.error(Pretty.pretty(result))
    }
  }
}
