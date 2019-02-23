sealed trait Set[E] extends (E => Boolean) {
  import Set.{ Cons, Empty, empty }

  final override def apply(input: E): Boolean = {
    var result = false

    foreach { current =>
      result = result || current == input
    }

    result
  }

  final def add(input: E): Set[E] = {
    var result = Cons(input, empty)

    foreach { current =>
      if (current != input) result = Cons(current, result)
    }

    result
  }

  final def remove(input: E): Set[E] = {
    var result = empty[E]

    foreach { current =>
      if (current != input) result = Cons(current, result)
    }

    result
  }

  final def union(that: Set[E]): Set[E] = {
    var result = that

    foreach { current =>
      result = result.add(current)
    }

    result
  }

  final def intersection(that: Set[E]): Set[E] = {
    var result = empty[E]

    foreach { current =>
      if (that(current)) result = result.add(current)
    }

    result
  }

  final def difference(that: Set[E]): Set[E] = {
    var result = empty[E]

    foreach { current =>
      if (!that(current)) result = result.add(current)
    }

    result
  }

  final def isSubsetOf(that: Set[E]): Boolean = {
    var result = true

    foreach { current =>
      result = result && that(current)
    }

    result
  }

  final def isSupersetOf(that: Set[E]): Boolean = that.isSubsetOf(this)

  final override def equals(other: Any): Boolean = other match {
    case that: Set[E] => this.isSubsetOf(that) && that.isSubsetOf(this)
    case _ => false
  }

  final override def hashCode: Int =
    if (isEmpty) 42
    else {
      val cons = this.asInstanceOf[Cons[E]]

      cons.element.hashCode + cons.otherElements.hashCode
    }

  final def size: Int = {
    var result = 0

    foreach { _ =>
      result += 1
    }

    result
  }

  final def isEmpty: Boolean = this.isInstanceOf[Empty[E]]

  final def nonEmpty: Boolean = !isEmpty

  def isSingleton: Boolean =
    if (isEmpty) false
    else {
      val cons = this.asInstanceOf[Cons[E]]

      cons.otherElements.isEmpty
    }

  def sample: Option[E] =
    if (isEmpty) None
    else {
      val cons = this.asInstanceOf[Cons[E]]

      Some(cons.element)
    }

  @scala.annotation.tailrec
  final def foreach[R](function: E => R): Unit = {
    if (nonEmpty) {
      val cons = this.asInstanceOf[Cons[E]]

      function(cons.element)
      cons.otherElements.foreach(function)
    }
  }

  final def map[R](function: E => R): Set[R] = {
    var result = empty[R]

    foreach { current =>
      result = result.add(function(current))
    }

    result
  }

  final def flatMap[R](function: E => Set[R]): Set[R] = {
    var result = empty[R]

    foreach { outerCurrent =>
      function(outerCurrent).foreach { innerCurrent =>
        result = result.add(innerCurrent)
      }
    }

    result
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
