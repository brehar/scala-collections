trait FoldableFactory[+E, A[+L] <: FoldableFactory[L, A]] extends Foldable[E] {
  protected def factory: Factory[A]

  def add[S >: E](input: S): A[S]

  def remove[S >: E](input: S): A[S]

  final def filterNot(predicate: E => Boolean): A[E] = filter(!predicate(_))

  def filter(predicate: E => Boolean): A[E] = fold[A[E]](factory.empty) { (acc, current) =>
    if (predicate(current)) acc.add(current)
    else acc
  }

  def map[R](function: E => R): A[R] = fold[A[R]](factory.empty)(_ add function(_))

  def flatMap[R](function: E => Foldable[R]): A[R] = fold[A[R]](factory.empty) { (acc, current) =>
    function(current).fold(acc)(_ add _)
  }
}
