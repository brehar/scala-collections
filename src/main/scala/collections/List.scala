package collections

import mathlib.{ Arbitrary, ClosedBinaryOperation, Gen, Monoid }

sealed abstract class List[+E] extends FoldableFactory[E, List] with (Int => Option[E]) {
  import List._

  final protected def factory: Factory[List] = List

  final def apply(index: Int): Option[E] = {
    @scala.annotation.tailrec
    def loop(list: List[E], count: Int): Option[E] =
      if (index < 0) None
      else if (count == index) list.head
      else loop(list.tail, count + 1)

    loop(this, 0)
  }

  final def isEmpty: Boolean = this.isInstanceOf[Nil.type]

  final def nonEmpty: Boolean = !isEmpty

  @scala.annotation.tailrec
  final def foldLeft[R](seed: R)(function: (R, => E) => R): R = this match {
    case Nil => seed
    case Cons(element, otherElements) =>
      val currentResult = function(seed, element)
      otherElements.foldLeft(currentResult)(function)
  }

  final def reduceLeft[R >: E](function: (R, => E) => R): Option[R] = head.map { seed =>
    tail.foldLeft[R](seed)(function)
  }

  final def reduceLeftOrThrowException[R >: E](function: (R, => E) => R): R =
    reduceLeft(function).get

  final override def foldRight[R](seed: => R)(function: (=> E, => R) => R): R = this match {
    case Nil => seed
    case Cons(element, otherElements) =>
      lazy val otherResult = otherElements.foldRight(seed)(function)
      function(element, otherResult)
  }

  final def reduceRight[R >: E](function: (=> E, => R) => R): Option[R] = head.map { seed =>
    tail.foldRight[R](seed)(function)
  }

  final def reduceRightOrThrowException[R >: E](function: (=> E, => R) => R): R =
    reduceRight(function).get

  final def :::[S >: E](that: List[S]): List[S] = that.foldRight[List[S]](this)(_ :: _)

  final def ::[S >: E](input: S): List[S] = Cons(input, this)

  @inline final def add[S >: E](input: S): List[S] = ::(input)

  final lazy val (head, tail) = popElement

  final def popElement: (Option[E], List[E]) = this match {
    case Nil => None -> empty
    case Cons(element, otherElements) => Some(element) -> otherElements
  }

  final def take(amount: Int): List[E] = {
    @scala.annotation.tailrec
    def loop(list: List[E], acc: List[E], count: Int): List[E] = list match {
      case Nil => acc
      case Cons(element, otherElements) =>
        if (count >= amount) acc
        else loop(otherElements, element :: acc, count + 1)
    }

    loop(this, empty, 0).reversed
  }

  final def reversed: List[E] = foldLeft[List[E]](empty) { (acc, current) =>
    current :: acc
  }

  final override def toString: String = s"List($toStringContent)"

  private[this] def toStringContent: String = this match {
    case Nil => ""
    case Cons(element, otherElements) => s"$element${otherElements.splitByCommaSpace}"
  }

  final def zip[T](that: List[T]): List[(E, T)] = this match {
    case Nil => Nil
    case Cons(element, otherElements) =>
      that match {
        case Nil => Nil
        case Cons(thatElement, thatOtherElements) =>
          (element -> thatElement) :: (otherElements zip thatOtherElements)
      }
  }

  final def interleave[S >: E](that: List[S]): List[S] = this match {
    case Nil => that
    case Cons(element, otherElements) => element :: that.interleave(otherElements)
  }
}

object List extends Factory[List] {
  final override def apply[E](element: E, otherElements: E*): List[E] =
    element :: otherElements.foldRight[List[E]](empty)(_ :: _)

  final def unapplySeq[E](list: List[E]): Option[Seq[E]] =
    if (list == null) None
    else Some(list.foldRight[scala.List[E]](scala.List.empty)(_ :: _))

  final case class Cons[+E](element: E, otherElements: List[E]) extends List[E]

  final case object Nil extends List[Nothing]

  def nothing: List[Nothing] = Nil

  implicit def arbitrary[T: Arbitrary]: Arbitrary[List[T]] = Arbitrary(gen[T])

  def gen[T: Arbitrary]: Gen[List[T]] = Gen.listOf(Arbitrary.arbitrary[T]).map {
    case scala.Nil => empty[T]
    case head :: tail => List(head, tail: _*)
  }

  def genNonEmpty[T: Arbitrary]: Gen[List[T]] = Gen.nonEmptyListOf(Arbitrary.arbitrary[T]).map {
    case scala.Nil => sys.error("should never happen")
    case head :: tail => List(head, tail: _*)
  }

  implicit def Concatenation[A: Arbitrary]: Monoid[List[A]] = new Monoid[List[A]] {
    final lazy val uniqueIdentityElement: List[A] = empty[A]
    final lazy val operation: ClosedBinaryOperation[List[A]] = _ ::: _
    final protected lazy val arbitrary: Arbitrary[List[A]] = implicitly[Arbitrary[List[A]]]
  }
}
