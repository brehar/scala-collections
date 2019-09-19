package collections

class FoldableTest extends TestStyle {
  test("contains") {
    Cell(1).contains(123) shouldBe false
    Cell(1).contains(1) shouldBe true
  }
}

case class Cell(input: Int) extends Foldable[Int] {
  final def foldLeft[R](seed: R)(function: (R, Int) => R): R = function(seed, input)
  final def foldRight[R](seed: => R)(function: (Int, => R) => R): R = function(input, seed)
}
