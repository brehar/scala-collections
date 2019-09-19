package collections

trait FoldableFactory[+E, A[+L] <: FoldableFactory[L, A]] extends Foldable[E] {
  protected def factory: Factory[A]

  def add[S >: E](input: S): A[S]

  final def filterNot(predicate: E => Boolean): A[E] = filter(!predicate(_))

  def filter(predicate: E => Boolean): A[E] = foldRight[A[E]](factory.empty) { (current, acc) =>
    if (predicate(current)) acc.add(current)
    else acc
  }

  def takeWhile(predicate: E => Boolean): A[E] = foldRight[A[E]](factory.empty) { (current, acc) =>
    if (predicate(current)) acc.add(current)
    else factory.empty
  }

  final def withFilter(predicate: E => Boolean): FoldableFactory.Wrapper[E, A] =
    new FoldableFactory.Wrapper(this, predicate)

  def map[R](function: E => R): A[R] = foldRight[A[R]](factory.empty) { (current, acc) =>
    acc.add(function(current))
  }

  def flatMap[R](function: E => Foldable[R]): A[R] = foldRight[A[R]](factory.empty) {
    (current, acc) =>
      function(current).foldRight(acc) { (current, acc) =>
        acc.add(current)
      }
  }

  def flatten[R](implicit view: E => Foldable[R]): A[R] = flatMap(view)
}

object FoldableFactory {
  final class Wrapper[+E, A[+L] <: FoldableFactory[L, A]](
    foldableFactory: FoldableFactory[E, A],
    predicate: E => Boolean) {
    def foreach[R](function: E => R): Unit = foldableFactory.foldLeft(()) { (_, current) =>
      if (predicate(current)) function(current)
    }

    def map[R](function: E => R): A[R] =
      foldableFactory.foldRight[A[R]](foldableFactory.factory.empty) { (current, acc) =>
        if (predicate(current)) acc.add(function(current))
        else acc
      }

    def flatMap[R, F[_]](function: E => F[R])(implicit view: F[R] => Foldable[R]): A[R] =
      foldableFactory.foldRight[A[R]](foldableFactory.factory.empty) { (current, acc) =>
        if (predicate(current)) view(function(current)).foldRight(acc) { (current, acc) =>
          acc.add(current)
        }
        else acc
      }
  }
}
