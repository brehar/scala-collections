package collections

sealed trait Set[+E] extends FoldableFactory[E, Set] {
  import Set.{ Cons, empty }

  final protected def factory: Factory[Set] = Set

  final def apply[S >: E](input: S): Boolean = contains(input)

  @scala.annotation.tailrec
  final def fold[R](seed: R)(function: (R, E) => R): R = {
    if (isEmpty) seed
    else otherElementsOrThrowException.fold(function(seed, elementOrThrowException))(function)
  }

  final def add[S >: E](input: S): Set[S] = fold(Cons(input, empty)) { (acc, current) =>
    if (current == input) acc
    else Cons(current, acc)
  }

  final def remove[S >: E](input: S): Set[S] = fold[Set[S]](empty) { (acc, current) =>
    if (current == input) acc
    else Cons(current, acc)
  }

  final def union[S >: E](that: Set[S]): Set[S] = fold(that)(_ add _)

  final def intersection[S >: E](that: Set[S]): Set[S] = filter(that)

  final def difference(predicate: E => Boolean): Set[E] = fold[Set[E]](empty) { (acc, current) =>
    if (predicate(current)) acc
    else acc.add(current)
  }

  final def isSubsetOf[S >: E](that: Set[S]): Boolean = forall(that)

  final def isSupersetOf[S >: E](that: Set[S]): Boolean = that.isSubsetOf(this)

  final override def equals(other: Any): Boolean = other match {
    case that: Set[E] => this.isSubsetOf(that) && that.isSubsetOf(this)
    case _ => false
  }

  final override def hashCode: Int = fold(42)(_ + _.hashCode())

  final override def toString: String =
    if (isEmpty) "{}"
    else {
      val otherElementsSplitByCommaSpace = otherElementsOrThrowException.fold("") {
        (acc, current) =>
          s"$acc, $current"
      }

      s"{ $elementOrThrowException$otherElementsSplitByCommaSpace }"
    }

  final def isEmpty: Boolean = this eq empty

  final def nonEmpty: Boolean = !isEmpty

  def isSingleton: Boolean = nonEmpty && otherElementsOrThrowException.isEmpty

  def sample: Option[E] =
    if (isEmpty) None
    else Some(elementOrThrowException)

  private[this] lazy val (elementOrThrowException, otherElementsOrThrowException) = {
    val cons = this.asInstanceOf[Cons[E]]

    cons.element -> cons.otherElements
  }
}

object Set extends Factory[Set] {
  private final case class Cons[E](element: E, otherElements: Set[E]) extends Set[E]

  private object Cons {
    private[this] def unapply(any: Any): Option[(String, Any)] = patternMatchingNotSupported
  }

  private object Empty extends Set[Nothing] {
    private[this] def unapply(any: Any): Option[(String, Any)] = patternMatchingNotSupported
  }

  private[this] def unapply(any: Any): Option[(String, Any)] = patternMatchingNotSupported

  private[this] def patternMatchingNotSupported: Nothing =
    sys.error("Pattern matching on Sets is expensive and therefore not supported.")

  final def empty: Set[Nothing] = Empty

  implicit def setCanBeUsedAsFunction1[E](set: Set[E]): E => Boolean = set.apply
}
