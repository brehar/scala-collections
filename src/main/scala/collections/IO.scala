package collections

final case class IO[+A](unsafeRun: () => A) {
  def map[B](f: A => B): IO[B] = IO.pure(f(unsafeRun()))
  def flatMap[B](f: A => IO[B]): IO[B] = IO.pure(f(unsafeRun()).unsafeRun())
}

object IO {
  def pure[A](a: => A): IO[A] = IO(() => a)
}
