package collections

import org.scalatest.{ FunSuite, Matchers }
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

trait TestStyle extends FunSuite with Matchers with ScalaCheckPropertyChecks
