package collections

trait Factory[A[+L] <: FoldableFactory[L, A]] {
  final def apply[E](element: E, otherElements: E*): A[E] =
    otherElements.foldLeft[A[E]](empty.add(element))(_ add _)

  final def empty[E]: A[E] = nothing

  def nothing: A[Nothing]
}
