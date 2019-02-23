import org.scalatest.{ FunSuite, Matchers }

class SetTest extends FunSuite with Matchers {
  test("apply on an empty Set should yield false") {
    Set.empty(randomString) shouldBe false
    Set.empty.size shouldBe 0
  }

  test("add on an empty Set should yield a new Set with one element") {
    val first = randomString
    val second = randomString

    first should not be second

    val set = Set(first)

    set(first) shouldBe true
    set(second) shouldBe false
  }

  test("add on a non-empty Set should yield a new Set with two elements") {
    val first = randomString
    val second = randomString

    first should not be second

    val set = Set(first, second)

    set(first) shouldBe true
    set(second) shouldBe true
  }

  test("remove on an empty Set should yield an empty Set") {
    val element = randomString
    val stillEmpty = Set.empty.remove(element)

    stillEmpty(element) shouldBe false
  }

  test("remove on a non-empty Set should yield a new Set without the removed element") {
    val element = randomString
    val setWithElement = Set(element)

    setWithElement(element) shouldBe true

    val setWithoutElement = setWithElement.remove(element)

    setWithoutElement(element) shouldBe false
  }

  test("remove removes only the element in question - with first added element removed") {
    val first = randomString
    val second = randomString
    val setWithTwoElements = Set(first, second)

    setWithTwoElements(first) shouldBe true
    setWithTwoElements(second) shouldBe true

    val setWithElementRemoved = setWithTwoElements.remove(first)

    setWithElementRemoved(first) shouldBe false
    setWithElementRemoved(second) shouldBe true
  }

  test("remove removes only the element in question - with second added element removed") {
    val first = randomString
    val second = randomString
    val setWithTwoElements = Set(first, second)

    setWithTwoElements(first) shouldBe true
    setWithTwoElements(second) shouldBe true

    val setWithElementRemoved = setWithTwoElements.remove(second)

    setWithElementRemoved(first) shouldBe true
    setWithElementRemoved(second) shouldBe false
  }

  test("add/remove combo should ensure that all elements are distinct") {
    val element = randomString
    val set = Set(element, element).remove(element)

    set(element) shouldBe false
  }

  test("union on an empty Set should yield an empty Set") {
    Set.empty.union(Set.empty) shouldBe Set.empty
  }

  test("union on a non-empty Set with an empty Set should yield the original Set untouched") {
    val first = randomString
    val second = randomString

    first should not be second

    val emptySet = Set.empty
    val nonEmptySet = Set(first, second)

    emptySet.union(nonEmptySet)(first) shouldBe true
    emptySet.union(nonEmptySet)(second) shouldBe true

    nonEmptySet.union(emptySet)(first) shouldBe true
    nonEmptySet.union(emptySet)(second) shouldBe true
  }

  test("union on two non-empty Sets should yield their union") {
    val a = randomString
    val b = randomString
    val c = randomString
    val d = randomString

    val left = Set(a, b)
    val right = Set(c, d)

    left.union(right) shouldBe Set(a, b, c, d)
    right.union(left) shouldBe Set(a, b, c, d)
  }

  test("intersection on empty Set should yield an empty Set") {
    Set.empty.intersection(Set.empty)(randomString) shouldBe false
  }

  test("intersection on a non-empty Set with an empty Set should yield an empty Set") {
    val first = randomString
    val second = randomString

    first should not be second

    val emptySet = Set.empty
    val nonEmptySet = Set(first, second)

    emptySet.intersection(nonEmptySet)(first) shouldBe false
    emptySet.intersection(nonEmptySet)(second) shouldBe false

    emptySet.intersection(emptySet)(first) shouldBe false
    emptySet.intersection(emptySet)(second) shouldBe false
  }

  test("intersection on two non-empty Sets should yield their intersection") {
    val a = randomString
    val b = randomString
    val c = randomString
    val d = randomString

    val left = Set(a, b, c)
    val right = Set(b, c, d)

    left.intersection(right) shouldBe Set(b, c)
    right.intersection(left) shouldBe Set(b, c)
  }

  test("difference on empty Set should yield an empty Set") {
    Set.empty.difference(Set.empty)(randomString) shouldBe false
  }

  test("difference on a non-empty Set with an empty Set should yield an empty Set") {
    val first = randomString
    val second = randomString

    first should not be second

    val emptySet = Set.empty
    val nonEmptySet = Set(first, second)

    emptySet.difference(nonEmptySet)(first) shouldBe false
    emptySet.difference(nonEmptySet)(second) shouldBe false

    nonEmptySet.difference(emptySet)(first) shouldBe true
    nonEmptySet.difference(emptySet)(second) shouldBe true
  }

  test("difference on two non-empty Sets should yield their difference") {
    val a = randomString
    val b = randomString
    val c = randomString
    val d = randomString

    val left = Set(a, b, c)
    val right = Set(b, c, d)

    left.difference(right) shouldBe Set(a)
    right.difference(left) shouldBe Set(d)
  }

  test("isSubsetOf on an empty Set should yield true") {
    Set.empty.isSubsetOf(Set.empty) shouldBe true
    Set.empty.isSubsetOf(Set(randomString)) shouldBe true
  }

  test("isSubsetOf on itself should yield true") {
    val set = Set(randomString)

    set.isSubsetOf(set) shouldBe true
  }

  test("isSubsetOf on a non-empty Set should yield false") {
    val a = randomString
    val b = randomString
    val c = randomString

    val left = Set(a, b)
    val right = Set(a, b, c)

    left.isSubsetOf(right) shouldBe true
    right.isSubsetOf(left) shouldBe false
  }

  test("isSupersetOf an empty Set should yield true") {
    Set.empty.isSupersetOf(Set.empty) shouldBe true
    Set(randomString).isSupersetOf(Set.empty) shouldBe true
  }

  test("isSupersetOf on itself should yield true") {
    val set = Set(randomString)

    set.isSupersetOf(set) shouldBe true
  }

  test("isSupersetOf on a non-empty Set should yield false") {
    val a = randomString
    val b = randomString
    val c = randomString

    val left = Set(a, b)
    val right = Set(a, b, c)

    left.isSupersetOf(right) shouldBe false
    right.isSupersetOf(left) shouldBe true
  }

  test("hashCode on an empty Set should not be random") {
    Set.empty.hashCode() shouldBe Set.empty.hashCode()

    val element = randomString

    Set(element).hashCode() shouldBe Set(element).hashCode()
  }

  test("hashCode on an empty Set should not be 0") {
    Set.empty.hashCode() should not be 0
  }

  test("hashCode on a non-empty Set should be the sum of all the hashCodes of the empty Set") {
    val first = randomString
    val second = randomString
    val expected = Set.empty.hashCode() + first.hashCode + second.hashCode

    Set(first, second).hashCode() shouldBe expected
  }

  test("size on an empty Set should be 0") {
    Set.empty.size shouldBe 0
  }

  test("size on a non-empty Set with 1 distinct element added should be 1") {
    Set(randomString).size shouldBe 1
  }

  test("size on a non-empty Set with 2 distinct elements added should be 2") {
    val first = randomString
    val second = randomString

    first should not be second

    Set(first, second).size shouldBe 2
  }

  test("size on a non-empty Set with 2 equal elements added should be 1") {
    val element = randomString

    Set(element, element).size shouldBe 1
  }

  test("isEmpty on an empty Set should yield true") {
    Set.empty.isEmpty shouldBe true
    Set.empty.nonEmpty shouldBe false
  }

  test("isEmpty on a non-empty Set should yield false") {
    Set(randomString).isEmpty shouldBe false
    Set(randomString).nonEmpty shouldBe true
  }

  test("isSingleton on an empty Set should yield false") {
    Set.empty.isSingleton shouldBe false
  }

  test("isSingleton on a Set with more than one element should yield false") {
    val first = randomString
    val second = randomString

    first should not be second

    Set(first, second).isSingleton shouldBe false
  }

  test("isSingleton on a Set with a single element should yield true") {
    Set(randomString).isSingleton shouldBe true
  }

  test("sample should yield a random element from the Set") {
    Set.empty.sample shouldBe None

    val a = randomString

    Set(a).sample shouldBe Some(a)

    val b = randomString

    Set(a, b).sample should contain oneOf (a, b)
  }

  test("foreach on an empty Set should not apply the function") {
    noException should be thrownBy Set.empty.foreach(_ => sys.error("should not be thrown"))
  }

  test("foreach on a non-empty Set should apply the function") {
    var functionWasApplied = false

    Set(randomString).foreach(_ => functionWasApplied = true)

    functionWasApplied shouldBe true
  }

  test("foreach should be able to calculate the size of an empty Set") {
    var size = 0
    val set = Set.empty

    set.foreach(_ => size += 1)

    size shouldBe 0
    size shouldBe set.size
  }

  test("foreach should be able to calculate the size of a non-empty Set with 1 element") {
    var size = 0
    val set = Set(randomString)

    set.foreach(_ => size += 1)

    size shouldBe 1
    size shouldBe set.size
  }

  test("foreach should be able to calculate the size of a non-empty Set with 2 elements") {
    var size = 0
    val a = randomString
    val b = randomString

    a should not be b

    val set = Set(a, b)

    set.foreach(_ => size += 1)

    size shouldBe 2
    size shouldBe set.size
  }

  test("foreach should be able to calculate the size of a non-empty Set with 1 unique element") {
    var size = 0
    val element = randomString
    val set = Set(element, element)

    set.foreach(_ => size += 1)

    size shouldBe 1
    size shouldBe set.size
  }

  test("Set() should not compile") {
    "Set()" shouldNot compile
  }

  test(
    "calling the varargs apply method on the Set companion object should yield a Set with all the arguments as elements") {
      val a = randomString
      val b = randomString
      val c = randomString

      Set(a, b, c) shouldBe Set.empty.add(a).add(b).add(c)
    }

  private def randomString: String = scala.util.Random.alphanumeric.take(5).mkString
}
