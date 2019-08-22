package collections

trait FoldableFactory2[K, +V, A[C, +D] <: FoldableFactory2[C, D, A]] extends Foldable[(K, V)] {
  protected def factory: Factory2[A]

  def add[S >: V](input: (K, S)): A[K, S]

  final def filterNot(predicate: ((K, V)) => Boolean): A[K, V] = filter(!predicate(_))

  def filter(predicate: ((K, V)) => Boolean): A[K, V] = fold[A[K, V]](factory.empty) {
    (acc, current) =>
      if (predicate(current)) acc.add(current)
      else acc
  }

  final def withFilter(predicate: ((K, V)) => Boolean): FoldableFactory2.Wrapper[K, V, A] =
    new FoldableFactory2.Wrapper(this, predicate)

  def map[L, R](function: ((K, V)) => (L, R)): A[L, R] =
    fold[A[L, R]](factory.empty)(_ add function(_))

  def flatMap[L, R](function: ((K, V)) => Foldable[(L, R)]): A[L, R] =
    fold[A[L, R]](factory.empty) { (acc, current) =>
      function(current).fold(acc)(_ add _)
    }
}

object FoldableFactory2 {
  final class Wrapper[K, +V, A[C, +D] <: FoldableFactory2[C, D, A]](
    foldableFactory: FoldableFactory2[K, V, A],
    predicate: ((K, V)) => Boolean) {
    def foreach[R](function: ((K, V)) => R): Unit = foldableFactory.fold(()) { (_, current) =>
      if (predicate(current)) function(current)
    }

    def map[L, R](function: ((K, V)) => (L, R)): A[L, R] =
      foldableFactory.fold[A[L, R]](foldableFactory.factory.empty) { (acc, current) =>
        if (predicate(current)) acc.add(function(current))
        else acc
      }

    def flatMap[L, R](function: ((K, V)) => Foldable[(L, R)]): A[L, R] =
      foldableFactory.fold[A[L, R]](foldableFactory.factory.empty) { (acc, current) =>
        if (predicate(current)) function(current).fold(acc)(_ add _)
        else acc
      }
  }
}
