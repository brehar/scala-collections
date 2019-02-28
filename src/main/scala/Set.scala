sealed trait Set[+E] {
  import Set.{ Cons, empty }

  final def apply[S >: E](input: S): Boolean = contains(input)

  final def doesNotContain[S >: E](input: S): Boolean = !contains(input)

  final def contains[S >: E](input: S): Boolean = exists(_ == input)

  final def doesNotExist(predicate: E => Boolean): Boolean = !exists(predicate)

  final def exists(predicate: E => Boolean): Boolean = fold(false)(_ || predicate(_))

  final def notForall(predicate: E => Boolean): Boolean = !forall(predicate)

  final def forall(predicate: E => Boolean): Boolean = fold(true)(_ && predicate(_))

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

  final def filter(predicate: E => Boolean): Set[E] = fold[Set[E]](empty) { (acc, current) =>
    if (predicate(current)) acc.add(current)
    else acc
  }

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

  final def size: Int = fold(0) { (acc, _) =>
    acc + 1
  }

  final def isEmpty: Boolean = this eq empty

  final def nonEmpty: Boolean = !isEmpty

  def isSingleton: Boolean = nonEmpty && otherElementsOrThrowException.isEmpty

  def sample: Option[E] =
    if (isEmpty) None
    else Some(elementOrThrowException)

  @scala.annotation.tailrec
  final def fold[R](seed: R)(function: (R, E) => R): R = {
    if (isEmpty) seed
    else otherElementsOrThrowException.fold(function(seed, elementOrThrowException))(function)
  }

  final def foreach[R](function: E => R): Unit = fold(()) { (_, current) =>
    function(current)
  }

  final def map[R](function: E => R): Set[R] = fold[Set[R]](empty)(_ add function(_))

  final def flatMap[R](function: E => Set[R]): Set[R] = fold[Set[R]](empty) { (acc, current) =>
    function(current).fold(acc)(_ add _)
  }

  private[this] lazy val (elementOrThrowException, otherElementsOrThrowException) = {
    val cons = this.asInstanceOf[Cons[E]]

    cons.element -> cons.otherElements
  }
}

object Set {
  def apply[E](element: E, otherElements: E*): Set[E] =
    otherElements.foldLeft[Set[E]](empty.add(element))(_ add _)

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

  def empty: Set[Nothing] = Empty

  implicit def setCanBeUsedAsFunction1[E](set: Set[E]): E => Boolean = set.apply
}
