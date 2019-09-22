package collections

final case class IO[+A](unsafeRun: () => A) {
  import IO._

  lazy val memoized: IO[A] = {
    if (isMemoizationDisabled) this
    else {
      lazy val a = unsafeRun()
      pure(a)
    }
  }

  def map[B](f: A => B): IO[B] = pure(f(unsafeRun()))
  def flatMap[B](f: A => IO[B]): IO[B] = pure(f(unsafeRun()).unsafeRun())
}

object IO {
  private[this] var _isMemoizationEnabled = false

  def enableMemoization(): Unit = _isMemoizationEnabled = true
  def disableMemoization(): Unit = _isMemoizationEnabled = false

  def isMemoizationEnabled: Boolean = _isMemoizationEnabled
  def isMemoizationDisabled: Boolean = !isMemoizationEnabled

  def pure[A](a: => A): IO[A] = IO(() => a)
}
