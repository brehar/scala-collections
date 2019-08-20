package collections

trait FoldableFactory[+E, A[+L] <: FoldableFactory[L, A]] extends Foldable[E] {
  protected def factory: Factory[A]

  def add[S >: E](input: S): A[S]

  final def filterNot(predicate: E => Boolean): A[E] = filter(!predicate(_))

  def filter(predicate: E => Boolean): A[E] = fold[A[E]](factory.empty) { (acc, current) =>
    if (predicate(current)) acc.add(current)
    else acc
  }

  final def withFilter(predicate: E => Boolean): FoldableFactory.Wrapper[E, A] =
    new FoldableFactory.Wrapper(this, predicate)

  def map[R](function: E => R): A[R] = fold[A[R]](factory.empty)(_ add function(_))

  def flatMap[R](function: E => Foldable[R]): A[R] = fold[A[R]](factory.empty) { (acc, current) =>
    function(current).fold(acc)(_ add _)
  }
}

object FoldableFactory {
  final class Wrapper[+E, A[+L] <: FoldableFactory[L, A]](
    foldableFactory: FoldableFactory[E, A],
    predicate: E => Boolean) {
    def foreach[R](function: E => R): Unit = foldableFactory.fold(()) { (_, current) =>
      if (predicate(current)) function(current)
    }

    def map[R](function: E => R): A[R] =
      foldableFactory.fold[A[R]](foldableFactory.factory.empty) { (acc, current) =>
        if (predicate(current)) acc.add(function(current))
        else acc
      }

    def flatMap[R](function: E => Foldable[R]): A[R] =
      foldableFactory.fold[A[R]](foldableFactory.factory.empty) { (acc, current) =>
        if (predicate(current)) function(current).fold(acc)(_ add _)
        else acc
      }
  }
}
