package collections

import Trampoline.{ done, tailCall }

sealed abstract class Set[+E] extends FoldableFactory[E, Set] {
  import Set.empty

  final protected def factory: Factory[Set] = Set

  final def apply[S >: E](input: S): Boolean = contains(input)

  final override def contains[S >: E](input: S): Boolean = {
    @scala.annotation.tailrec
    def loop(stack: Stack[Set[E]]): Boolean = stack match {
      case Stack.Empty => false
      case Stack.Cons(set, otherSetsOnTheStack) =>
        set match {
          case Set.Empty() => loop(otherSetsOnTheStack)
          case Set.Cons(left, element, right) =>
            if (input == element) true
            else if (input.hashCode() <= element.hashCode()) loop(otherSetsOnTheStack.push(left))
            else loop(otherSetsOnTheStack.push(right))
        }
    }

    loop(Stack.empty.push(this))
  }

  final def fold[R](seed: R)(function: (R, E) => R): R = {
    @scala.annotation.tailrec
    def loop(stack: Stack[Set[E]], acc: R): R = stack match {
      case Stack.Empty => acc
      case Stack.Cons(set, otherSetsOnTheStack) =>
        set match {
          case Set.Empty() => loop(otherSetsOnTheStack, acc)
          case Set.Cons(left, element, right) =>
            loop(otherSetsOnTheStack.push(right).push(left), function(acc, element))
        }
    }

    loop(Stack.empty.push(this), seed)
  }

  final def add[S >: E](input: S): Set[S] = {
    @scala.annotation.tailrec
    def loop(set: Set[E], continuation: Set[S] => Trampoline[Set[S]]): Trampoline[Set[S]] =
      set match {
        case Set.Empty() => continuation(Set.Cons(empty, input, empty))
        case cons @ Set.Cons(left, element, right) =>
          if (input == element) continuation(cons)
          else if (input.hashCode() <= element.hashCode())
            loop(left, acc => tailCall(continuation(cons.copy(left = acc))))
          else loop(right, acc => tailCall(continuation(cons.copy(right = acc))))
      }

    loop(this, done).result
  }

  final def remove[S >: E](input: S): Set[S] = {
    @scala.annotation.tailrec
    def loop(set: Set[E], continuation: Set[S] => Set[S]): Set[S] = set match {
      case Set.Empty() => continuation(empty)
      case cons @ Set.Cons(left, element, right) =>
        if (input == element) continuation(left.union(right))
        else if (input.hashCode() <= element.hashCode())
          loop(left, acc => continuation(cons.copy(left = acc)))
        else loop(right, acc => continuation(cons.copy(right = acc)))
    }

    loop(this, identity)
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
      case Set.Empty() => ""
      case Set.Cons(left, element, right) =>
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
