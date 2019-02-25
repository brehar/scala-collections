sealed trait Set[E] extends (E => Boolean) {
  import Set.{ Cons, Empty, empty }

  final override def apply(input: E): Boolean = fold(false)(_ || _ == input)

  final def add(input: E): Set[E] = fold(Cons(input, empty)) { (acc, current) =>
    if (current == input) acc
    else Cons(current, acc)
  }

  final def remove(input: E): Set[E] = fold(empty[E]) { (acc, current) =>
    if (current == input) acc
    else Cons(current, acc)
  }

  final def union(that: Set[E]): Set[E] = fold(that)(_ add _)

  final def intersection(that: Set[E]): Set[E] = fold(empty[E]) { (acc, current) =>
    if (that(current)) acc.add(current)
    else acc
  }

  final def difference(that: Set[E]): Set[E] = fold(empty[E]) { (acc, current) =>
    if (that(current)) acc
    else acc.add(current)
  }

  final def isSubsetOf(that: Set[E]): Boolean = fold(true)(_ && that(_))

  final def isSupersetOf(that: Set[E]): Boolean = that.isSubsetOf(this)

  final override def equals(other: Any): Boolean = other match {
    case that: Set[E] => this.isSubsetOf(that) && that.isSubsetOf(this)
    case _ => false
  }

  final override def hashCode: Int = fold(42)(_ + _.hashCode())

  final def size: Int = fold(0) { (acc, _) => acc + 1 }

  final def isEmpty: Boolean = this.isInstanceOf[Empty[E]]

  final def nonEmpty: Boolean = !isEmpty

  def isSingleton: Boolean =
    if (isEmpty) false
    else this.asInstanceOf[Cons[E]].otherElements.isEmpty

  def sample: Option[E] =
    if (isEmpty) None
    else Some(this.asInstanceOf[Cons[E]].element)

  @scala.annotation.tailrec
  final def fold[R](seed: R)(function: (R, E) => R): R = {
    if (isEmpty) seed
    else {
      val cons = this.asInstanceOf[Cons[E]]

      cons.otherElements.fold(function(seed, cons.element))(function)
    }
  }

  final def foreach[R](function: E => R): Unit = fold(()) { (_, current) => function(current) }

  final def map[R](function: E => R): Set[R] = fold(empty[R])(_ add function(_))

  final def flatMap[R](function: E => Set[R]): Set[R] = fold(empty[R]) { (acc, current) =>
    function(current).fold(acc)(_ add _)
  }
}

object Set {
  def apply[E](element: E, otherElements: E*): Set[E] = {
    var result: Set[E] = empty[E].add(element)

    otherElements.foreach { current =>
      result = result.add(current)
    }

    result
  }

  private final case class Cons[E](element: E, otherElements: Set[E]) extends Set[E]

  private object Cons {
    private[this] def unapply(any: Any): Option[(String, Any)] = patternMatchingNotSupported
  }

  private class Empty[E] extends Set[E] {
    private[this] def unapply(any: Any): Option[(String, Any)] = patternMatchingNotSupported
  }

  private[this] def unapply(any: Any): Option[(String, Any)] = patternMatchingNotSupported

  private[this] def patternMatchingNotSupported: Nothing =
    sys.error("Pattern matching on Sets is expensive and therefore not supported.")

  def empty[E]: Set[E] = new Empty[E]
}
