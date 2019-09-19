package collections

sealed abstract class List[+E] extends FoldableFactory[E, List] {
  import List._

  final protected def factory: Factory[List] = List

  @scala.annotation.tailrec
  final def foldLeft[R](seed: R)(function: (R, E) => R): R = this match {
    case Nil => seed
    case Cons(element, otherElements) =>
      val currentResult = function(seed, element)
      otherElements.foldLeft(currentResult)(function)
  }

  final override def foldRight[R](seed: => R)(function: (E, => R) => R): R = this match {
    case Nil => seed
    case Cons(element, otherElements) =>
      lazy val otherResult = otherElements.foldRight(seed)(function)
      function(element, otherResult)
  }

  final def :::[S >: E](that: List[S]): List[S] = that.foldRight[List[S]](this)(_ :: _)

  final def ::[S >: E](input: S): List[S] = Cons(input, this)

  @inline final def add[S >: E](input: S): List[S] = ::(input)

  final lazy val (head, tail) = popElement

  final def popElement: (Option[E], List[E]) = this match {
    case Nil => None -> empty
    case Cons(element, otherElements) => Some(element) -> otherElements
  }

  final def reversed: List[E] = foldLeft[List[E]](empty) { (acc, current) =>
    current :: acc
  }

  final override def toString: String = s"List($toStringContent)"

  private[this] def toStringContent: String = this match {
    case Nil => ""
    case Cons(element, otherElements) => s"$element${otherElements.splitByCommaSpace}"
  }
}

object List extends Factory[List] {
  final override def apply[E](element: E, otherElements: E*): List[E] =
    element :: otherElements.foldRight[List[E]](empty)(_ :: _)

  final case class Cons[+E](element: E, otherElements: List[E]) extends List[E]

  final case object Nil extends List[Nothing]

  def nothing: List[Nothing] = Nil
}
