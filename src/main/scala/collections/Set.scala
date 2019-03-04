package collections

sealed abstract class Set[+E] extends FoldableFactory[E, Set] {
  import Set.Color.Red
  import Set.{ Color, Cons, Empty, Okasaki, empty }

  final protected def factory: Factory[Set] = Set

  final def apply[S >: E](input: S): Boolean = contains(input)

  @scala.annotation.tailrec
  final override def contains[S >: E](input: S): Boolean = this match {
    case Empty() => false
    case Cons(_, left, element, right) =>
      if (input == element) true
      else if (input.hashCode() <= element.hashCode()) left.contains(input)
      else right.contains(input)
  }

  final def fold[R](seed: R)(function: (R, E) => R): R = this match {
    case Empty() => seed
    case Cons(_, left, element, right) =>
      val currentResult = function(seed, element)
      val rightResult = right.fold(currentResult)(function)

      left.fold(rightResult)(function)
  }

  final def add[S >: E](input: S): Set[S] = {
    val okasaki = new Okasaki[S]

    def ins(set: Set[S]): Set[S] = set match {
      case Empty() => Cons(Red, empty, input, empty)
      case Cons(color, left, element, right) =>
        if (input == element) set
        else if (input.hashCode() <= element.hashCode())
          okasaki.balance(Cons(color, ins(left), element, right))
        else okasaki.balance(Cons(color, left, element, ins(right)))
    }

    okasaki.makeBlack(ins(this))
  }

  final def remove[S >: E](input: S): Set[S] = this match {
    case Empty() => empty
    case Cons(color, left, element, right) =>
      if (input == element) left.union(right)
      else if (input.hashCode() <= element.hashCode())
        Cons(color, left.remove(input), element, right)
      else Cons(color, left, element, right.remove(input))
  }

  final def union[S >: E](that: Set[S]): Set[S] = fold(that)(_ add _)

  final def intersection[S >: E](that: Set[S]): Set[S] = filter(that)

  final def difference(predicate: E => Boolean): Set[E] = fold[Set[E]](empty) { (acc, current) =>
    if (predicate(current)) acc
    else acc.add(current)
  }

  final def isSubsetOf[S >: E](that: Set[S]): Boolean = forall(that)

  final def isSupersetOf[S >: E](that: Set[S]): Boolean = that.isSubsetOf(this)

  final override def equals(other: Any): Boolean = other match {
    case that: Set[E] => this.isSubsetOf(that) && that.isSubsetOf(this)
    case _ => false
  }

  final override def hashCode: Int = fold(42)(_ + _.hashCode())

  final def isEmpty: Boolean = this eq empty

  final def nonEmpty: Boolean = !isEmpty

  def isSingleton: Boolean

  def sample: Option[E]

  final def rendered: String = {
    def leftOrRight(isLeft: Boolean, isFirst: Boolean): String =
      if (isFirst) ""
      else if (isLeft) "└── "
      else "├── "

    def leftOrRightParent(isLeft: Boolean, isFirst: Boolean): String =
      if (isFirst) ""
      else if (isLeft) "    "
      else "|   "

    def loop(prefix: String, isLeft: Boolean, isFirst: Boolean, set: Set[E]): String = set match {
      case Empty() => ""
      case Cons(color, left, element, right) =>
        prefix + leftOrRight(isLeft, isFirst) + color.paint(element) + "\n" + loop(
          prefix + leftOrRightParent(isLeft, isFirst),
          isLeft = false,
          isFirst = false,
          right) + loop(
            prefix + leftOrRightParent(isLeft, isFirst),
            isLeft = true,
            isFirst = false,
            left)
    }

    loop("", isLeft = true, isFirst = true, this)
  }

  protected def color: Color
}

object Set extends Factory[Set] {
  private final case class Cons[+E](color: Color, left: Set[E], element: E, right: Set[E])
    extends Set[E] {
    def isSingleton: Boolean = left.isEmpty && right.isEmpty

    def sample: Option[E] = Some(element)

    override def toString: String =
      "{ " + element + splitByCommaSpace(left) + splitByCommaSpace(right) + " }"

    private[this] def splitByCommaSpace(input: Set[E]): String = input.fold("") { (acc, current) =>
      s"$acc, $current"
    }
  }

  private object Empty extends Set[Nothing] {
    def unapply[E](set: Set[E]): Boolean = set.isInstanceOf[Empty.type]

    final def isSingleton: Boolean = false

    final def sample: Option[Nothing] = None

    final override def toString: String = "{}"

    final def color: Color = Color.Black
  }

  final def empty: Set[Nothing] = Empty

  private[Set] sealed abstract class Color {
    def paint(element: Any): String
  }

  private[Set] object Color {
    case object Red extends Color {
      final def paint(element: Any): String = Console.RED + element + Console.RESET
    }

    case object Black extends Color {
      final def paint(element: Any): String = Console.BLUE + element + Console.RESET
    }
  }

  private[Set] class Okasaki[S] {
    import Set.Color.{ Black, Red }

    private def solution[E](a: Set[E], b: Set[E], c: Set[E], d: Set[E], x: E, y: E, z: E): Set[E] =
      Cons(Red, Cons(Black, a, x, b), y, Cons(Black, c, z, d))

    def makeBlack(set: Set[S]): Set[S] = set match {
      case Empty() => set
      case Cons(_, left, element, right) => Cons(Black, left, element, right)
    }

    private type Case = PartialFunction[Set[S], Set[S]]

    private lazy val top: Case = {
      case Cons(Black, Cons(Red, a, x, Cons(Red, b, y, c)), z, d) => solution(a, b, c, d, x, y, z)
    }

    private lazy val right: Case = {
      case Cons(Black, a, x, Cons(Red, b, y, Cons(Red, c, z, d))) => solution(a, b, c, d, x, y, z)
    }

    private lazy val bottom: Case = {
      case Cons(Black, a, x, Cons(Red, Cons(Red, b, y, c), z, d)) => solution(a, b, c, d, x, y, z)
    }

    private lazy val left: Case = {
      case Cons(Black, Cons(Red, Cons(Red, a, x, b), y, c), z, d) => solution(a, b, c, d, x, y, z)
    }

    private lazy val center: Case = {
      case balanced => balanced
    }

    lazy val balance: Case = top orElse right orElse bottom orElse left orElse center
  }

  implicit def setCanBeUsedAsFunction1[E](set: Set[E]): E => Boolean = set.apply
}

object Main extends App {
  println("-" * 50)

  println(Set(0, 1 to 19: _*).rendered)

  println("-" * 50)
}
