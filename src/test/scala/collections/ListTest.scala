package collections

import mathlib.{ IntAddition, IntMultiplication, Semigroup }

class ListTest extends TestStyle {
  test(
    "Calling the varargs apply method on the List companion object should yield a List with all the arguments as elements, which are appropriately ordered") {
      forAll { (a: Int, b: Int, c: Int) =>
        List(a, b, c) shouldBe a :: b :: c :: List.empty
      }
    }

  test("Calling reversed on a List should reverse the list") {
    forAll { original: List[Int] =>
      original.reversed.reversed shouldBe original
    }
  }

  test("toString on an empty List should yield List()") {
    List.empty.toString shouldBe "List()"
  }

  test("toString on a List with one element should yield List(element)") {
    forAll { element: String =>
      List(element).toString shouldBe s"List($element)"
    }
  }

  test("toString on a List with two elements should yield List(first, second)") {
    forAll { (first: Int, second: Int) =>
      List(first, second).toString shouldBe s"List($first, $second)"
      List(second, first).toString shouldBe s"List($second, $first)"
    }
  }

  test("concatenation") {
    List.empty ::: List.empty shouldBe List.empty

    forAll { (a: Int, b: Int, c: Int, d: Int) =>
      val left = List(a, b)
      val right = List(c, d)
      val sum = List(a, b, c, d)

      left ::: List.empty shouldBe left
      List.empty ::: right shouldBe right
      left ::: right shouldBe sum
    }
  }

  test("takeWhile") {
    val list = List(1, 2, 3, 4, 5, 6)

    list.takeWhile(_ % 2 != 0) shouldBe List(1)
    list.takeWhile(_ < 3) shouldBe List(1, 2)
    list.takeWhile(_ <= 3) shouldBe List(1, 2, 3)
  }

  test("find") {
    val list = List(1, 2, 3, 4, 5, 6)

    list.find(_ % 2 != 0) shouldBe Some(1)
    list.find(_ % 2 == 0) shouldBe Some(2)
    list.find(_ < 3) shouldBe Some(1)
    list.find(_ <= 3) shouldBe Some(1)
    list.find(_ >= 7) shouldBe None
  }

  test("filter") {
    forAll { original: List[Int] =>
      original.filter(_ => true) shouldBe original
    }
  }

  test("map") {
    forAll { (a: Int, b: Int, c: Int, f: Int => Int) =>
      List(a, b, c).map(f) shouldBe List(f(a), f(b), f(c))
    }
  }

  test("flatMap") {
    forAll { (a: Char, b: Char, y: Int, z: Int) =>
      List(a, b).flatMap { c =>
        List(y, z).map { n =>
          c -> n
        }
      } shouldBe List(a -> y, a -> z, b -> y, b -> z)
    }
  }

  test("flatten") {
    forAll { distinct: Distinct.Four[Int] =>
      val Distinct.Four(a, b, c, d) = distinct

      List(Set(a, b), Set(c, d)).flatten shouldBe List(a, b, c, d)
      List(List(a, b), List(c, d)).flatten shouldBe List(a, b, c, d)
    }
  }

  test("Group Theory") {
    val listOfIntConcatenation: Semigroup[List[Int]] = Semigroup[List[Int]]
    val listOfStringConcatenation: Semigroup[List[String]] = List.Concatenation[String]
  }

  test("fold should be able to express aggregation") {
    forAll { (a: Int, b: Int, c: Int) =>
      List(a, b, c).foldLeft(0)(_ + _) shouldBe a + b + c
      List(a, b, c).foldRight(0)(_ + _) shouldBe a + b + c
    }

    forAll { (a: String, b: String, c: String) =>
      List(a, b, c).foldLeft("")(_ + _) shouldBe a + b + c
      List(a, b, c).foldRight("")(_ + _) shouldBe a + b + c
    }

    forAll { (a: List[Int], b: List[Int], c: List[Int]) =>
      List(a, b, c).foldLeft(List.empty[Int])(_ ::: _) shouldBe a ::: b ::: c
      List(a, b, c).foldRight(List.empty[Int])(_ ::: _) shouldBe a ::: b ::: c
    }

    List(2, 4).aggregated shouldBe 6
    List(2, 4).aggregated(IntAddition) shouldBe 6
    List(2, 4).aggregated(IntMultiplication) shouldBe 8
  }
}
