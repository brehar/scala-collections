package collections

sealed abstract class Trampoline[+R] {
  import Trampoline._

  @scala.annotation.tailrec
  final def result: R = this match {
    case Done(res) => res
    case Thunk(thunk) => thunk().result
    case FlatMap(trampoline, continuation) =>
      trampoline match {
        case Done(res) => continuation(res).result
        case Thunk(thunk) => thunk().flatMap(continuation).result
        case FlatMap(innerTrampoline, innerContinuation) =>
          innerTrampoline.flatMap { r =>
            innerContinuation(r).flatMap(continuation)
          }.result
      }
  }

  final def map[T](continuation: R => T): Trampoline[T] =
    flatMap(result => done(continuation(result)))

  final def flatMap[T](continuation: R => Trampoline[T]): Trampoline[T] = this match {
    case self: FlatMap[r, _] =>
      val innerTrampoline = self.trampoline
      val innerContinuation = self.continuation

      FlatMap(innerTrampoline, { r: r =>
        innerContinuation(r).flatMap(continuation)
      })
    case trampoline => FlatMap(trampoline, continuation)
  }
}

object Trampoline {
  private final case class Done[+R](res: R) extends Trampoline[R]
  private final case class Thunk[+R](thunk: () => Trampoline[R]) extends Trampoline[R]
  private final case class FlatMap[T, +R](
    trampoline: Trampoline[T],
    continuation: T => Trampoline[R])
    extends Trampoline[R]

  def done[R](res: R): Trampoline[R] = Done(res)
  def tailCall[R](thunk: => Trampoline[R]): Trampoline[R] = Thunk(() => thunk)
}
