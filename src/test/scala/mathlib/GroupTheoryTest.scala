package mathlib

import collections.TestStyle

class GroupTheoryTest extends TestStyle {
  test("Group Theory") {
    val intAddition: Ring[Int] = IntAddition
    val intMultiplication: Monoid[Int] = IntMultiplication

    val booleanAddition: Rig[Boolean] = BooleanAddition
    val booleanMultiplication: Rig[Boolean] = BooleanMultiplication

    val stringConcatenation: Monoid[String] = StringConcatenation
  }
}
