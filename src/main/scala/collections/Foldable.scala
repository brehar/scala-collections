package collections

trait Foldable[+E] {
  def fold[R](seed: R)(function: (R, E) => R): R

  def size: Int = fold(0) { (acc, _) =>
    acc + 1
  }

  final def doesNotContain[S >: E](input: S): Boolean = !contains(input)

  def contains[S >: E](input: S): Boolean = exists(_ == input)

  final def doesNotExist(predicate: E => Boolean): Boolean = !exists(predicate)

  def exists(predicate: E => Boolean): Boolean = fold(false)(_ || predicate(_))

  final def notForall(predicate: E => Boolean): Boolean = !forall(predicate)

  def forall(predicate: E => Boolean): Boolean = fold(true)(_ && predicate(_))

  final def foreach[R](function: E => R): Unit = fold(()) { (_, current) =>
    function(current)
  }
}
