sealed trait Set extends (String => Boolean) {
  import Set.{ Cons, empty }

  final override def apply(input: String): Boolean = {
    var result = false

    foreach { current =>
      result = result || current == input
    }

    result
  }

  final def add(input: String): Set = {
    var result = Cons(input, empty)

    foreach { current =>
      if (current != input) result = Cons(current, result)
    }

    result
  }

  final def remove(input: String): Set = {
    var result = empty

    foreach { current =>
      if (current != input) result = Cons(current, result)
    }

    result
  }

  final def union(that: Set): Set = {
    var result = that

    foreach { current =>
      result = result.add(current)
    }

    result
  }

  final def intersection(that: Set): Set = {
    var result = empty

    foreach { current =>
      if (that(current)) result = result.add(current)
    }

    result
  }

  final def difference(that: Set): Set = {
    var result = empty

    foreach { current =>
      if (!that(current)) result = result.add(current)
    }

    result
  }

  final def isSubsetOf(that: Set): Boolean = {
    var result = true

    foreach { current =>
      result = result && that(current)
    }

    result
  }

  final def isSupersetOf(that: Set): Boolean = that.isSubsetOf(this)

  final override def equals(other: Any): Boolean = other match {
    case that: Set => this.isSubsetOf(that) && that.isSubsetOf(this)
    case _ => false
  }

  final override def hashCode: Int =
    if (isEmpty) super.hashCode()
    else {
      val cons = this.asInstanceOf[Cons]

      cons.element.hashCode + cons.otherElements.hashCode
    }

  final def size: Int = {
    var result = 0

    foreach { _ =>
      result += 1
    }

    result
  }

  final def isEmpty: Boolean = this eq Set.empty

  final def nonEmpty: Boolean = !isEmpty

  def isSingleton: Boolean =
    if (isEmpty) false
    else {
      val cons = this.asInstanceOf[Cons]

      cons.otherElements.isEmpty
    }

  def sample: Option[String] =
    if (isEmpty) None
    else {
      val cons = this.asInstanceOf[Cons]

      Some(cons.element)
    }

  @scala.annotation.tailrec
  final def foreach(function: String => Unit): Unit = {
    if (nonEmpty) {
      val cons = this.asInstanceOf[Cons]

      function(cons.element)
      cons.otherElements.foreach(function)
    }
  }
}

object Set {
  def apply(element: String, otherElements: String*): Set = {
    var result: Set = empty.add(element)

    otherElements.foreach { current =>
      result = result.add(current)
    }

    result
  }

  private final case class Cons(element: String, otherElements: Set) extends Set

  private object Cons {
    private[this] def unapply(any: Any): Option[(String, Set)] = patternMatchingNotSupported
  }

  private object Empty extends Set {
    private[this] def unapply(any: Any): Option[(String, Set)] = patternMatchingNotSupported
  }

  private[this] def unapply(any: Any): Option[(String, Set)] = patternMatchingNotSupported

  private[this] def patternMatchingNotSupported: Nothing =
    sys.error("Pattern matching on Sets is expensive and therefore not supported.")

  val empty: Set = Empty
}
