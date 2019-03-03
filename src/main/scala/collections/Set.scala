package collections

sealed abstract class Set[+E] extends FoldableFactory[E, Set] {
  import Set.{ Cons, Empty, empty }

  final protected def factory: Factory[Set] = Set

  final def apply[S >: E](input: S): Boolean = contains(input)

  @scala.annotation.tailrec
  final def fold[R](seed: R)(function: (R, E) => R): R = this match {
    case _: Empty.type => seed
    case cons: Cons[E] => cons.otherElements.fold(function(seed, cons.element))(function)
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

  final def isEmpty: Boolean = this eq empty

  final def nonEmpty: Boolean = !isEmpty

  def isSingleton: Boolean

  def sample: Option[E]
}

object Set extends Factory[Set] {
  private final case class Cons[E](element: E, otherElements: Set[E]) extends Set[E] {
    def isSingleton: Boolean = otherElements.isEmpty

    def sample: Option[E] = Some(element)

    override def toString: String =
      "{ " + element + otherElementsSplitByCommaSpace(otherElements) + " }"

    private[this] def otherElementsSplitByCommaSpace(input: Set[E]): String = input.fold("") {
      (acc, current) =>
        s"$acc, $current"
    }
  }

  private object Cons {
    // $COVERAGE-OFF$
    private[this] def unapply(any: Any): Option[(String, Any)] = patternMatchingNotSupported
    // $COVERAGE-ON$
  }

  private object Empty extends Set[Nothing] {
    final def isSingleton: Boolean = false

    final def sample: Option[Nothing] = None

    final override def toString: String = "{}"

    // $COVERAGE-OFF$
    private[this] def unapply(any: Any): Option[(String, Any)] = patternMatchingNotSupported
    // $COVERAGE-ON$
  }

  // $COVERAGE-OFF$
  private[this] def unapply(any: Any): Option[(String, Any)] = patternMatchingNotSupported
  // $COVERAGE-ON$

  // $COVERAGE-OFF$
  private[this] def patternMatchingNotSupported: Nothing =
    sys.error("Pattern matching on Sets is expensive and therefore not supported.")
  // $COVERAGE-ON$

  final def empty: Set[Nothing] = Empty

  implicit def setCanBeUsedAsFunction1[E](set: Set[E]): E => Boolean = set.apply
}
