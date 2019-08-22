package collections

trait Factory2[A[K, +V] <: FoldableFactory2[K, V, A]] {
  final def apply[K, V](element: (K, V), otherElements: (K, V)*): A[K, V] =
    otherElements.foldLeft[A[K, V]](empty.add(element))(_ add _)

  def empty[K, V]: A[K, V]
}
