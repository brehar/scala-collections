package collections

sealed abstract class Trampoline[+R] {
  import Trampoline.{ Done, Thunk }

  @scala.annotation.tailrec
  final def result: R = this match {
    case Done(res) => res
    case Thunk(thunk) => thunk().result
  }
}

object Trampoline {
  private final case class Done[+R](res: R) extends Trampoline[R]
  private final case class Thunk[+R](thunk: () => Trampoline[R]) extends Trampoline[R]

  def done[R](res: R): Trampoline[R] = Done(res)
  def tailCall[R](thunk: => Trampoline[R]): Trampoline[R] = Thunk(() => thunk)
}
