package collections

sealed abstract class Tree[+E] extends FoldableFactory[E, Tree] {
  import Tree._

  final protected def factory: Factory[Tree] = Tree

  @scala.annotation.tailrec
  final override def contains[S >: E](input: S): Boolean = this match {
    case Empty => false
    case NonEmpty(left, element, right) =>
      if (input == element) true
      else if (input.hashCode() <= element.hashCode()) left.contains(input)
      else right.contains(input)
  }

  final def foldLeft[R](seed: R)(function: (R, => E) => R): R = this match {
    case Empty => seed
    case NonEmpty(left, element, right) =>
      val currentResult = function(seed, element)
      val rightResult = right.foldLeft(currentResult)(function)
      left.foldLeft(rightResult)(function)
  }

  final def foldRight[R](seed: => R)(function: (=> E, => R) => R): R = this match {
    case Empty => seed
    case NonEmpty(left, element, right) =>
      lazy val leftResult = left.foldRight(seed)(function)
      lazy val rightResult = right.foldRight(leftResult)(function)
      function(element, rightResult)
  }

  final def add[S >: E](input: S): Tree[S] = this match {
    case Empty => NonEmpty(empty, input, empty)
    case nonEmpty @ NonEmpty(left, element, right) =>
      if (input.hashCode() <= element.hashCode()) nonEmpty.copy(left = left.add(input))
      else nonEmpty.copy(right = right.add(input))
  }

  final def remove[S >: E](input: S): Tree[S] = this match {
    case Empty => empty
    case nonEmpty @ NonEmpty(left, element, right) =>
      if (input == element) left.remove(input).union(right)
      else if (input.hashCode() <= element.hashCode()) nonEmpty.copy(left = left.remove(input))
      else nonEmpty.copy(right = right.remove(input))
  }

  final def union[S >: E](that: Tree[S]): Tree[S] = foldLeft(that)(_ add _)

  final override def hashCode: Int = foldLeft(42)(_ + _.hashCode())

  final def isEmpty: Boolean = this eq empty

  final def nonEmpty: Boolean = !isEmpty

  final def rendered: String = {
    def leftOrRight(isLeft: Boolean, isFirst: Boolean): String =
      if (isFirst) ""
      else if (isLeft) "└── "
      else "├── "

    def leftOrRightParent(isLeft: Boolean, isFirst: Boolean): String =
      if (isFirst) ""
      else if (isLeft) "    "
      else "|   "

    def loop(prefix: String, isLeft: Boolean, isFirst: Boolean, tree: Tree[E]): String =
      tree match {
        case Tree.Empty => ""
        case Tree.NonEmpty(left, element, right) =>
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

object Tree extends Factory[Tree] {
  final case class NonEmpty[+E](left: Tree[E], element: E, right: Tree[E]) extends Tree[E] {
    override def productPrefix: String = "Tree.NonEmpty"
  }

  final object Empty extends Tree[Nothing] {
    override def toString: String = "Tree.Empty"
  }

  final def nothing: Tree[Nothing] = Empty
}
