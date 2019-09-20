import org.scalacheck.Prop._

package object mathlib {
  type ClosedBinaryOperation[A] = (A, A) => A

  private[mathlib] type Law = org.scalacheck.Prop

  type Arbitrary[A] = org.scalacheck.Arbitrary[A]
  type Gen[A] = org.scalacheck.Gen[A]

  @inline def Arbitrary: org.scalacheck.Arbitrary.type = org.scalacheck.Arbitrary
  @inline def Gen: org.scalacheck.Gen.type = org.scalacheck.Gen

  final implicit class InfixNotation[A](private val a1: A) extends AnyVal {
    @inline def combine(a2: A)(implicit magma: Magma[A]): A = magma.operation(a1, a2)
  }

  def closure[A: Arbitrary: Magma](operation: ClosedBinaryOperation[A]): Law = forAll {
    (a1: A, a2: A) =>
      val _: A = a1 combine a2
      true
  }

  def associativity[A: Arbitrary: Magma](operation: ClosedBinaryOperation[A]): Law = forAll {
    (a1: A, a2: A, a3: A) =>
      val chaining = (a1 combine a2) combine a3
      val nesting = a1 combine (a2 combine a3)

      chaining == nesting
  }

  def identity[A: Arbitrary: Magma](
    operation: ClosedBinaryOperation[A],
    uniqueIdentityElement: A): Law = forAll { a: A =>
    Predef.identity(a) == a && a.combine(uniqueIdentityElement) == a && uniqueIdentityElement
      .combine(a) == a
  }

  def invertability[A: Arbitrary: Magma](
    operation: ClosedBinaryOperation[A],
    uniqueIdentityElement: A,
    uniqueInverseElement: A => A): Law = forAll { a: A =>
    a.combine(uniqueInverseElement(a)) == uniqueIdentityElement && uniqueInverseElement(a).combine(
      a) == uniqueIdentityElement
  }

  def commutativity[A: Arbitrary: Magma](operation: ClosedBinaryOperation[A]): Law = forAll {
    (a1: A, a2: A) =>
      a1.combine(a2) == a2.combine(a1)
  }

  def distributivity[A: Arbitrary](
    plus: ClosedBinaryOperation[A],
    times: ClosedBinaryOperation[A]): Law = forAll { (a1: A, a2: A, a3: A) =>
    times(a1, plus(a2, a3)) == plus(times(a1, a2), times(a1, a3)) && times(plus(a2, a3), a1) == plus(
      times(a2, a1),
      times(a3, a1))
  }

  implicit lazy val IntAddition: Ring[Int] = new Ring[Int] {
    final lazy val that: Monoid[Int] = IntMultiplication
    final lazy val uniqueInverseElement: Int => Int = -_
    final lazy val uniqueIdentityElement: Int = 0
    final lazy val operation: ClosedBinaryOperation[Int] = _ + _
    final protected lazy val arbitrary: Arbitrary[Int] = implicitly[Arbitrary[Int]]
  }

  implicit lazy val IntMultiplication: Monoid[Int] = new Monoid[Int] {
    final lazy val uniqueIdentityElement: Int = 1
    final lazy val operation: ClosedBinaryOperation[Int] = _ * _
    final protected lazy val arbitrary: Arbitrary[Int] = implicitly[Arbitrary[Int]]
  }

  implicit lazy val StringConcatenation: Monoid[String] = new Monoid[String] {
    final lazy val uniqueIdentityElement: String = ""
    final lazy val operation: ClosedBinaryOperation[String] = _ + _
    final protected lazy val arbitrary: Arbitrary[String] = implicitly[Arbitrary[String]]
  }

  trait BooleanAdditionMonoid extends Monoid[Boolean] {
    final lazy val uniqueIdentityElement: Boolean = false
    final lazy val operation: ClosedBinaryOperation[Boolean] = _ || _
    final protected lazy val arbitrary: Arbitrary[Boolean] = implicitly[Arbitrary[Boolean]]
  }

  object BooleanAdditionMonoid extends BooleanAdditionMonoid

  implicit object BooleanAddition extends Rig[Boolean] with BooleanAdditionMonoid {
    final lazy val that: Monoid[Boolean] = BooleanMultiplicationMonoid
  }

  trait BooleanMultiplicationMonoid extends Monoid[Boolean] {
    final lazy val uniqueIdentityElement: Boolean = true
    final lazy val operation: ClosedBinaryOperation[Boolean] = _ && _
    final protected lazy val arbitrary: Arbitrary[Boolean] = implicitly[Arbitrary[Boolean]]
  }

  object BooleanMultiplicationMonoid extends BooleanMultiplicationMonoid

  implicit object BooleanMultiplication extends Rig[Boolean] with BooleanMultiplicationMonoid {
    final lazy val that: Monoid[Boolean] = BooleanAdditionMonoid
  }
}
