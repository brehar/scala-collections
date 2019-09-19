package collections

trait Foldable[+E] {
  def foldLeft[R](seed: R)(function: (R, E) => R): R

  def foldRight[R](seed: => R)(function: (E, => R) => R): R

  def size: Int = foldLeft(0) { (acc, _) =>
    acc + 1
  }

  final def doesNotContain[S >: E](input: S): Boolean = !contains(input)

  def contains[S >: E](input: S): Boolean = exists(_ == input)

  final def doesNotExist(predicate: E => Boolean): Boolean = !exists(predicate)

  def exists(predicate: E => Boolean): Boolean = foldRight(false)(predicate(_) || _)

  final def notForall(predicate: E => Boolean): Boolean = !forall(predicate)

  def forall(predicate: E => Boolean): Boolean = foldLeft(true)(_ && predicate(_))

  def foreach[R](function: E => R): Unit = foldLeft(()) { (_, current) =>
    function(current)
  }

  final def groupBy[K](key: E => K): Map[K, Set[E]] = foldLeft[Map[K, Set[E]]](Map.empty) {
    (acc, current) =>
      val k: K = key(current)
      val value: Set[E] = acc(k).map(_.add(current)).getOrElse(Set(current))

      acc.add(k -> value)
  }

  final def splitByCommaSpace: String = foldLeft("") { (acc, current) =>
    s"$acc, $current"
  }

  def find(predicate: E => Boolean): Option[E] = foldRight[Option[E]](None) { (current, acc) =>
    if (predicate(current)) Some(current)
    else acc
  }
}

object Foldable {
  implicit def viewFromTraversableToFoldableFromCollections[E](from: Traversable[E]): Foldable[E] =
    new Foldable[E] {
      final def foldLeft[R](seed: R)(function: (R, E) => R): R = from.foldLeft(seed)(function)

      final def foldRight[R](seed: => R)(function: (E, => R) => R): R = from.foldRight(seed) {
        (current, acc) =>
          function(current, acc)
      }
    }

  implicit def viewFromFoldableToTraversableFromCollections[E](from: Foldable[E]): Traversable[E] =
    new Traversable[E] {
      final def foreach[R](function: E => R): Unit = from.foreach(function)
    }
}
