package collections

import org.scalatest.{ FunSuite, Matchers }

class MapJoinTest extends FunSuite with Matchers {
  import MapJoin.DSL
  import MapJoinTest._

  test("inner join") {
    people.join(cars).inner shouldBe Map(1 -> ("alfa", "audi"), 3 -> ("charlie", "cadillac"))

    people.join(cars).inner(PersonWithCarBrand) shouldBe Map(
      1 -> PersonWithCarBrand("alfa", "audi"),
      3 -> PersonWithCarBrand("charlie", "cadillac"))

    val colors = Map(3 -> "blue")

    people.join(cars).inner.join(colors).inner shouldBe Map(
      3 -> (("charlie", "cadillac") -> "blue"))
  }

  test("left outer join") {
    people.join(cars).leftOuter shouldBe Map(
      1 -> ("alfa", Some("audi")),
      3 -> ("charlie", Some("cadillac")),
      4 -> ("delta", None))
  }

  test("left only join") {
    people.join(cars).leftOnly shouldBe Map(4 -> ("delta", None))
  }

  test("right outer join") {
    people.join(cars).rightOuter shouldBe Map(
      1 -> (Some("alfa"), "audi"),
      2 -> (None, "bmw"),
      3 -> (Some("charlie"), "cadillac"))
  }

  test("right only join") {
    people.join(cars).rightOnly shouldBe Map(2 -> (None, "bmw"))
  }

  test("full outer join") {
    people.join(cars).fullOuter shouldBe Map(
      1 -> (Some("alfa"), Some("audi")),
      2 -> (None, Some("bmw")),
      3 -> (Some("charlie"), Some("cadillac")),
      4 -> (Some("delta"), None))
  }

  test("outer join") {
    people.join(cars).outer shouldBe Map(2 -> (None, Some("bmw")), 4 -> (Some("delta"), None))
  }
}

object MapJoinTest {
  type PersonId = Int
  type PersonName = String
  type CarBrand = String

  val people: Map[PersonId, PersonName] = Map(1 -> "alfa", 3 -> "charlie", 4 -> "delta")
  val cars: Map[PersonId, CarBrand] = Map(1 -> "audi", 2 -> "bmw", 3 -> "cadillac")

  final case class PersonWithCarBrand(name: String, carBrand: String)
}
