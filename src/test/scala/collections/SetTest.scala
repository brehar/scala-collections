package collections

import org.scalatest.{ FunSuite, Matchers }

class SetTest extends FunSuite with Matchers {
  import SetTest._

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

  test("adding the same element twice should simply ignore the input") {
    val first = randomString
    val second = randomString

    first should not be second

    Set(first, second, second) shouldBe Set(first, second)
    Set(first, second, second).size shouldBe 2

    Set(first, second, first) shouldBe Set(first, second)
    Set(first, second, first).size shouldBe 2
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

  test("remove should remove elements from both sides of the tree") {
    Set(1, 2, 3).remove(3) shouldBe Set(1, 2)
    Set(1, -2, -3).remove(-3) shouldBe Set(1, -2)
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

  test("union with variance") {
    val (employee, consultant) = bothRoles

    Set(employee).add(consultant) shouldBe Set(employee, consultant)
    Set(employee).union(Set(consultant)) shouldBe Set(employee, consultant)
  }

  test("intersection on empty Set should yield an empty Set") {
    Set.empty.intersection(Set.nothing) shouldBe Set.empty
    Set.empty.filter(Set.nothing) shouldBe Set.empty
    Set.nothing.filter(_ => false) shouldBe Set.empty
    Set.empty.filterNot(Set.nothing) shouldBe Set.empty
    Set.nothing.filterNot(_ => false) shouldBe Set.empty
  }

  test("intersection on a non-empty Set with an empty Set should yield an empty Set") {
    val first = randomString
    val second = randomString

    first should not be second

    val emptySet = Set.empty
    val nonEmptySet = Set(first, second)

    emptySet.intersection(nonEmptySet)(first) shouldBe false
    emptySet.intersection(nonEmptySet)(second) shouldBe false
    emptySet.filter(nonEmptySet)(first) shouldBe false
    emptySet.filter(nonEmptySet)(second) shouldBe false
    emptySet.filterNot(nonEmptySet)(first) shouldBe false
    emptySet.filterNot(nonEmptySet)(second) shouldBe false

    nonEmptySet.intersection(emptySet)(first) shouldBe false
    nonEmptySet.intersection(emptySet)(second) shouldBe false
    nonEmptySet.filter(emptySet)(first) shouldBe false
    nonEmptySet.filter(emptySet)(second) shouldBe false
    nonEmptySet.filterNot(emptySet)(first) shouldBe true
    nonEmptySet.filterNot(emptySet)(second) shouldBe true
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
    Set.empty.difference(Set.nothing) shouldBe Set.empty
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

  test("difference on two sets with different types should yield a Set with the common type") {
    val (employee, consultant) = bothRoles
    val employeeSet = Set(employee)
    val consultantSet = Set(consultant)

    employeeSet.difference(consultantSet) shouldBe employeeSet
    consultantSet.difference(employeeSet) shouldBe consultantSet
  }

  test("isSubsetOf on an empty Set should yield true") {
    Set.empty.isSubsetOf(Set.nothing) shouldBe true
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

  test("equals should be reflexive") {
    def reflexive(x: Any): Unit = {
      x shouldBe x
      x.hashCode() shouldBe x.hashCode()
    }

    reflexive(Set.empty)
    reflexive(Set(1))
    reflexive(Set(1, 2))
    reflexive(Set(2, 1))
  }

  test("equals should be symmetric") {
    def symmetric(x: Any, y: Any): Unit = {
      x shouldBe y
      y shouldBe x

      x.hashCode() shouldBe y.hashCode()
      y.hashCode() shouldBe x.hashCode()
    }

    symmetric(Set.empty, Set.empty)
    symmetric(Set(1), Set(1))
    symmetric(Set(1, 2), Set(1, 2))
    symmetric(Set(1, 2), Set(2, 1))
    symmetric(Set(1, 2, 3), Set(1, 2, 3))
    symmetric(Set(1, 2, 3), Set(1, 3, 2))
    symmetric(Set(1, 2, 3), Set(2, 1, 3))
    symmetric(Set(1, 2, 3), Set(2, 3, 1))
    symmetric(Set(1, 2, 3), Set(3, 1, 2))
    symmetric(Set(1, 2, 3), Set(3, 2, 1))
  }

  test("equals should be transitive") {
    def transitive(x: Any, y: Any, z: Any): Unit = {
      x shouldBe y
      y shouldBe z
      x shouldBe z

      x.hashCode() shouldBe y.hashCode()
      y.hashCode() shouldBe z.hashCode()
      x.hashCode() shouldBe z.hashCode()
    }

    transitive(Set.empty, Set.empty, Set.empty)
    transitive(Set(1, 2, 3), Set(3, 2, 1), Set(2, 1, 3))
  }

  test("these should not be equal") {
    Set(1) should not be Set(2)
    Set(2) should not be Set(1)
    Set(1) should not be Set(1, 2)
    Set(1, 2) should not be Set(1)
    Set(1) should not be Set(2, 1)
    Set(2, 1) should not be Set(1)
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

  test("foreach on an empty Set should not apply the function") {
    noException should be thrownBy Set.nothing.foreach(_ => sys.error("should not be thrown"))
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

  test(
    "foreach should be parameterized in the result of the argument function so that it does not produce warnings") {
      Set.nothing.foreach(_ => 1)
    }

  test("map on an empty Set should not apply the function") {
    noException should be thrownBy Set.nothing.map(_ => sys.error("should not be thrown"))
  }

  test("map should produce a Set") {
    Set("hello", "world").map(_.reverse) shouldBe Set("dlrow", "olleh")
  }

  test("map should be able to produce a Set of something other than String") {
    Set("hello", "world").map(_.length) shouldBe Set(5)
    Set("hello", "planet").map(_.length) shouldBe Set(5, 6)
  }

  test("flatMap should be able to produce a chess board") {
    val characters = Set('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
    val numbers = Set(1, 2, 3, 4, 5, 6, 7, 8)

    val chessBoard: Set[(Char, Int)] = characters.flatMap { c =>
      numbers.map { n =>
        c -> n
      }
    }

    chessBoard.size shouldBe 64
  }

  test("Set should be a function") {
    val orderedClassmates = Seq("alice", "bob", "frank")

    def isFriend(potentialFriend: String): Boolean =
      potentialFriend == "frank" || potentialFriend == "bob"

    orderedClassmates.filter(isFriend) shouldBe Seq("bob", "frank")

    val friends = Set("frank", "bob")

    orderedClassmates.filter(friends) shouldBe Seq("bob", "frank")
    orderedClassmates.filter(isFriend) shouldBe orderedClassmates.filter(friends)
  }

  test("contains on an empty Set should yield false") {
    Set.nothing.contains(randomString) shouldBe false
    Set.nothing.doesNotContain(randomString) shouldBe true
  }

  test("exists on an empty Set should yield false") {
    Set.nothing.exists(_ => false) shouldBe false
    Set.nothing.doesNotExist(_ => false) shouldBe true
  }

  test("exists on a non-empty Set should yield true") {
    val element = randomString

    Set(element).exists(_.length == element.length) shouldBe true
    Set(element).exists(_.length != element.length) shouldBe false

    Set(element).doesNotExist(_.length == element.length) shouldBe false
    Set(element).doesNotExist(_.length != element.length) shouldBe true
  }

  test("exists with variance") {
    val employee = randomEmployee

    Set(employee).exists(_ == employee) shouldBe true

    Set[Employee](employee).exists(_ == employee) shouldBe true
    Set[CompanyRole](employee).exists(_ == employee) shouldBe true

    Set[Employee](employee).exists((input: Employee) => input == employee) shouldBe true
    Set[Employee](employee).exists((input: CompanyRole) => input == employee) shouldBe true
    Set[CompanyRole](employee).exists((input: CompanyRole) => input == employee) shouldBe true
    "Set[CompanyRole](employee).exists((input: Employee) => input == employee)" shouldNot typeCheck

    Set[Employee](employee).exists(Set[Employee](employee)) shouldBe true
    Set[Employee](employee).exists(Set[CompanyRole](employee)) shouldBe true
    Set[CompanyRole](employee).exists(Set[CompanyRole](employee)) shouldBe true
    Set[CompanyRole](employee).exists(Set[Employee](employee)) shouldBe true
  }

  test("forall on an empty Set should yield false") {
    Set.nothing.forall(_ => false) shouldBe true
    Set.nothing.notForall(_ => false) shouldBe false
  }

  test("forall on a non-empty Set should yield true") {
    val element = randomString

    Set(element).forall(_.length == element.length) shouldBe true
    Set(element).forall(_.length != element.length) shouldBe false

    Set(element).notForall(_.length == element.length) shouldBe false
    Set(element).notForall(_.length != element.length) shouldBe true
  }

  test("toString on an empty Set should yield {}") {
    Set.empty.toString shouldBe "{}"
  }

  test("toString on a Set with one element should yield {element}") {
    val element = randomString

    Set(element).toString shouldBe s"{ $element }"
  }

  test("toString on a Set with two elements should yield {elementOne, elementTwo}") {
    val first = randomString
    val second = randomString

    first should not be second

    val actual = Set(first, second).toString

    actual.count(_ == '{') shouldBe 1
    actual should include(first)
    actual.count(_ == ',') shouldBe 1
    actual should include(second)
    actual.count(_ == '}') shouldBe 1
  }

  test("toString on a Set with three elements should yield {elementOne, elementTwo, elementThree}") {
    val first = randomString
    val second = randomString
    val third = randomString

    first should not be second
    second should not be third

    val actual = Set(first, second, third).toString

    actual.count(_ == '{') shouldBe 1
    actual should include(first)
    actual.count(_ == ',') shouldBe 2
    actual should include(second)
    actual should include(third)
    actual.count(_ == '}') shouldBe 1
  }

  test("toString should not produce any commas with leading spaces") {
    Set(1, 0).toString should not include " ,"
  }

  test("groupBy on an empty Set should produce an empty Map") {
    Set.empty[Int].groupBy(_.toString) shouldBe Map.empty[String, Set[Int]]
  }

  test("groupBy on a nonempty Set should produce a Map from key: K to value: Set[E]") {
    Set(1, 2, 3, 4).groupBy(_ % 2 == 0) shouldBe Map(true -> Set(2, 4), false -> Set(1, 3))

    val alice = Person("Alice", 27)
    val bob = Person("Bob", 25)
    val carol = Person("Carol", 27)
    val daniel = Person("Daniel", 27)

    val peopleWhoAreXYearsOld =
      Set(alice, bob, carol, daniel).groupBy(_.age).withDefaultValue(Set.empty)

    peopleWhoAreXYearsOld shouldBe Map(25 -> Set(bob), 27 -> Set(alice, carol, daniel))
    peopleWhoAreXYearsOld(10) shouldBe Some(Set.empty)
    peopleWhoAreXYearsOld(25) shouldBe Some(Set(bob))
    peopleWhoAreXYearsOld(27) shouldBe Some(Set(alice, carol, daniel))

    peopleWhoAreXYearsOld.map {
      case (age, people) => age -> people.size
    } shouldBe Map(25 -> 1, 27 -> 3)

    peopleWhoAreXYearsOld.mapValues(_.size) shouldBe Map(25 -> 1, 27 -> 3)
  }
}

object SetTest {
  final case class Person(name: String, age: Int)
}
