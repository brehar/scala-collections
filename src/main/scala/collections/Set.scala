package collections

final class Set[+E] private (val tree: Tree[E]) extends FoldableFactory[E, Set] {
  import Set._

  protected def factory: Factory[Set] = Set

  def apply[S >: E](input: S): Boolean = contains(input)

  override def contains[S >: E](input: S): Boolean = tree.contains(input)

  def foldLeft[R](seed: R)(function: (R, E) => R): R = tree.foldLeft(seed)(function)

  def foldRight[R](seed: => R)(function: (E, => R) => R): R = tree.foldRight(seed)(function)

  def add[S >: E](input: S): Set[S] =
    if (contains(input)) this
    else Set(tree add input)

  def remove[S >: E](input: S): Set[S] = Set(tree remove input)

  def union[S >: E](that: Set[S]): Set[S] = Set(this.tree union that.tree)

  def intersection[S >: E](that: Set[S]): Set[S] = filter(that)

  def difference(predicate: E => Boolean): Set[E] = foldLeft[Set[E]](empty) { (acc, current) =>
    if (predicate(current)) acc
    else acc.add(current)
  }

  def isSubsetOf[S >: E](that: Set[S]): Boolean = forall(that)

  def isSupersetOf[S >: E](that: Set[S]): Boolean = that.isSubsetOf(this)

  override def equals(other: Any): Boolean = other match {
    case that: Set[E] => this.isSubsetOf(that) && that.isSubsetOf(this)
    case _ => false
  }

  override def hashCode: Int = foldLeft(42)(_ + _.hashCode())

  override def toString: String = s"Set($toStringContent)"

  private[this] def toStringContent: String = tree match {
    case Tree.Empty => ""
    case Tree.NonEmpty(left, element, right) =>
      s"$element${left.splitByCommaSpace}${right.splitByCommaSpace}"
  }

  def isEmpty: Boolean = tree.isEmpty

  def nonEmpty: Boolean = !isEmpty

  def isSingleton: Boolean = tree match {
    case Tree.NonEmpty(Tree.Empty, _, Tree.Empty) => true
    case _ => false
  }

  def sample: Option[E] = tree match {
    case Tree.Empty => None
    case Tree.NonEmpty(_, element, _) => Some(element)
  }
}

object Set extends Factory[Set] {
  final def nothing: Set[Nothing] = apply(Tree.empty)

  private def apply[E](tree: Tree[E]): Set[E] = new Set(tree)

  implicit def setCanBeUsedAsFunction1[E](set: Set[E]): E => Boolean = set.apply
}
