package collections

sealed abstract class Set[+E] extends FoldableFactory[E, Set] {
  import Set.{ Empty, NonEmpty, empty }

  final protected def factory: Factory[Set] = Set

  final def apply[S >: E](input: S): Boolean = contains(input)

  @scala.annotation.tailrec
  final override def contains[S >: E](input: S): Boolean = this match {
    case Empty => false
    case NonEmpty(left, element, right) =>
      if (input == element) true
      else if (input.hashCode() <= element.hashCode()) left.contains(input)
      else right.contains(input)
  }

  final def fold[R](seed: R)(function: (R, E) => R): R = this match {
    case Empty() => seed
    case NonEmpty(left, element, right) =>
      val currentResult = function(seed, element)
      val rightResult = right.fold(currentResult)(function)
      left.fold(rightResult)(function)
  }

  final def add[S >: E](input: S): Set[S] = this match {
    case Empty => NonEmpty(empty, input, empty)
    case nonEmpty @ NonEmpty(left, element, right) =>
      if (input == element) nonEmpty
      else if (input.hashCode() <= element.hashCode()) nonEmpty.copy(left = left.add(input))
      else nonEmpty.copy(right = right.add(input))
  }

  final def remove[S >: E](input: S): Set[S] = this match {
    case Empty => empty
    case nonEmpty @ NonEmpty(left, element, right) =>
      if (input == element) left.union(right)
      else if (input.hashCode() <= element.hashCode()) nonEmpty.copy(left = left.remove(input))
      else nonEmpty.copy(right = right.remove(input))
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

  override def toString: String = this match {
    case Empty() => "{}"
    case NonEmpty(left, element, right) =>
      "{ " + element + splitByCommaSpace(left) + splitByCommaSpace(right) + " }"
  }

  private[this] def splitByCommaSpace(input: Set[E]): String = input.fold("") { (acc, current) =>
    s"$acc, $current"
  }

  final def isEmpty: Boolean = this eq empty

  final def nonEmpty: Boolean = !isEmpty

  final def isSingleton: Boolean = this match {
    case Empty() => false
    case NonEmpty(left, _, right) => left.isEmpty && right.isEmpty
  }

  final def sample: Option[E] = this match {
    case Empty() => None
    case NonEmpty(_, element, _) => Some(element)
  }

  final def rendered: String = {
    def leftOrRight(isLeft: Boolean, isFirst: Boolean): String =
      if (isFirst) ""
      else if (isLeft) "└── "
      else "├── "

    def leftOrRightParent(isLeft: Boolean, isFirst: Boolean): String =
      if (isFirst) ""
      else if (isLeft) "    "
      else "|   "

    def loop(prefix: String, isLeft: Boolean, isFirst: Boolean, set: Set[E]): String = set match {
      case Set.Empty() => ""
      case Set.NonEmpty(left, element, right) =>
        prefix + leftOrRight(isLeft, isFirst) + element + "\n" + loop(
          prefix + leftOrRightParent(isLeft, isFirst),
          isLeft = false,
          isFirst = false,
          right) + loop(
            prefix + leftOrRightParent(isLeft, isFirst),
            isLeft = true,
            isFirst = false,
            left)
    }

    loop("", isLeft = true, isFirst = true, this)
  }
}

object Set extends Factory[Set] {
  private final case class NonEmpty[+E](left: Set[E], element: E, right: Set[E]) extends Set[E]

  private object Empty extends Set[Nothing] {
    def unapply[E](set: Set[E]): Boolean = set.isInstanceOf[Empty.type]
  }

  final def nothing: Set[Nothing] = Empty

  implicit def setCanBeUsedAsFunction1[E](set: Set[E]): E => Boolean = set.apply
}
