package collections

sealed abstract class Stack[+E] {
  import Stack.{ Cons, Empty, empty }

  final def push[S >: E](input: S): Stack[S] = Cons(input, this)

  final lazy val (peek, pop) = popElement

  final def popElement: (Option[E], Stack[E]) = this match {
    case Empty => None -> empty
    case Cons(element, otherElements) => Some(element) -> otherElements
  }
}

object Stack {
  final case class Cons[+E](element: E, otherElements: Stack[E]) extends Stack[E]

  final case object Empty extends Stack[Nothing]

  def empty: Stack[Nothing] = Empty
}
