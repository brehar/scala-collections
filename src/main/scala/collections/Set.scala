package collections

sealed abstract class Set[+E] extends FoldableFactory[E, Set] {
  import Set.{ Cons, Empty, empty }

  final protected def factory: Factory[Set] = Set

  final def apply[S >: E](input: S): Boolean = contains(input)

  @scala.annotation.tailrec
  final override def contains[S >: E](input: S): Boolean = this match {
    case Empty() => false
    case Cons(left, element, right) =>
      if (input == element) true
      else if (input.hashCode() <= element.hashCode()) left.contains(input)
      else right.contains(input)
  }

  final def fold[R](seed: R)(function: (R, E) => R): R = this match {
    case Empty() => seed
    case Cons(left, element, right) =>
      val currentResult = function(seed, element)
      val rightResult = right.fold(currentResult)(function)

      left.fold(rightResult)(function)
  }

  final def add[S >: E](input: S): Set[S] = this match {
    case Empty() => Cons(empty, input, empty)
    case cons @ Cons(left, element, right) =>
      if (input == element) this
      else if (input.hashCode() <= element.hashCode()) cons.copy(left = left.add(input))
      else cons.copy(right = right.add(input))
  }

  final def remove[S >: E](input: S): Set[S] = this match {
    case Empty() => empty
    case cons @ Cons(left, element, right) =>
      if (input == element) left.union(right)
      else if (input.hashCode() <= element.hashCode()) cons.copy(left = left.remove(input))
      else cons.copy(right = right.remove(input))
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
      case Empty() => ""
      case Cons(left, element, right) =>
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
  private final case class Cons[+E](left: Set[E], element: E, right: Set[E]) extends Set[E] {
    def isSingleton: Boolean = left.isEmpty && right.isEmpty

    def sample: Option[E] = Some(element)

    override def toString: String =
      "{ " + element + splitByCommaSpace(left) + splitByCommaSpace(right) + " }"

    private[this] def splitByCommaSpace(input: Set[E]): String = input.fold("") { (acc, current) =>
      s"$acc, $current"
    }
  }

  private object Empty extends Set[Nothing] {
    def unapply[E](set: Set[E]): Boolean = set.isInstanceOf[Empty.type]

    final def isSingleton: Boolean = false

    final def sample: Option[Nothing] = None

    final override def toString: String = "{}"
  }

  final def empty: Set[Nothing] = Empty

  implicit def setCanBeUsedAsFunction1[E](set: Set[E]): E => Boolean = set.apply
}
