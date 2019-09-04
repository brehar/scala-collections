package collections

class ReferentialTransparencyTest extends TestStyle {
  test("Sets are referentially transparent") {
    val someValue = "HULK SMASH!!!"
    val setBeforeDb = Set(1, 2, 3)
    val setAfterDb = Set(3, 2, 1)
    val setBeforeDbMapped = setBeforeDb.map(_ => someValue)
    val setAfterDbMapped = setAfterDb.map(_ => someValue)

    setBeforeDbMapped shouldBe Set(someValue)
    setAfterDbMapped shouldBe Set(someValue)

    if (setBeforeDb == setAfterDb) setBeforeDbMapped shouldBe setAfterDbMapped
  }

  test("Maps are not referentially transparent") {
    import Map.PotentiallyDangerousImplicits._

    val someValue = "HULK SMASH!!!"
    val values: PartialFunction[Int, String] = {
      case 1 => "I"
      case 2 => "II"
      case 3 => "III"
    }

    val setBeforeDb = Set(1, 2, 3)
    val setAfterDb = Set(3, 2, 1)
    val mapBeforeDb = Map.withKeys(setBeforeDb).andSomeValues(values)
    val mapAfterDb = Map.withKeys(setAfterDb).andSomeValues(values)
    val mapBeforeDbMapped = mapBeforeDb.mapKeys(_ => someValue)
    val mapAfterDbMapped = mapAfterDb.mapKeys(_ => someValue)

    mapBeforeDbMapped shouldNot be(mapAfterDbMapped)
  }
}
