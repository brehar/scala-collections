package collections

import org.scalacheck.{ Arbitrary, Gen }

sealed abstract class Timeline[+E] extends (Int => Option[E]) {
  import Timeline._

  final def apply(index: Int): Option[E] = {
    @scala.annotation.tailrec
    def loop(timeline: Timeline[E], count: Int): Option[E] =
      if (index < 0) None
      else if (count == index) timeline.head
      else loop(timeline.tail, count + 1)

    loop(this, 0)
  }

  final def head: Option[E] = this match {
    case End => None
    case NonEmpty(recentEvent, _) => Some(recentEvent.unsafeRun())
  }

  final def tail: Timeline[E] = this match {
    case End => End
    case NonEmpty(_, followingEvents) => followingEvents.unsafeRun()
  }

  final def isEmpty: Boolean = this.isInstanceOf[End.type]

  final def nonEmpty: Boolean = !isEmpty

  @scala.annotation.tailrec
  final def foldLeft[R](seed: R)(function: (R, => E) => R): R = this match {
    case End => seed
    case NonEmpty(recentEvent, followingEvents) =>
      val currentResult = function(seed, recentEvent.unsafeRun())
      followingEvents.unsafeRun().foldLeft(currentResult)(function)
  }

  final def foldRight[R](seed: => R)(function: (=> E, => R) => R): R = this match {
    case End => seed
    case NonEmpty(recentEvent, followingEvents) =>
      lazy val followingResult = followingEvents.unsafeRun().foldRight(seed)(function)
      function(recentEvent.unsafeRun(), followingResult)
  }

  final def size: Int = foldLeft(0) { (acc, _) =>
    acc + 1
  }

  final def foreach[R](function: E => R): Unit = foldLeft(()) { (_, current) =>
    function(current)
  }

  final def map[R](function: E => R): Timeline[R] = foldRight[Timeline[R]](End)(function(_) #:: _)

  final def flatMap[R](function: (=> E) => Timeline[R]): Timeline[R] = foldRight[Timeline[R]](End) {
    (current, acc) =>
      function(current).foldRight(acc)(_ #:: _)
  }

  final def flatten[R](implicit view: (=> E) => Timeline[R]): Timeline[R] =
    foldRight[Timeline[R]](End) { (current, acc) =>
      view(current).foldRight(acc)(_ #:: _)
    }

  final def take(amount: Int): Timeline[E] = {
    @scala.annotation.tailrec
    def loop(timeline: Timeline[E], acc: Timeline[E], count: Int): Timeline[E] = timeline match {
      case End => acc
      case NonEmpty(recentEvent, followingEvents) =>
        if (count >= amount) acc
        else loop(followingEvents.unsafeRun(), recentEvent.unsafeRun() #:: acc, count + 1)
    }

    loop(this, End, 0).reversed
  }

  final def reversed: Timeline[E] = foldLeft[Timeline[E]](End) { (acc, current) =>
    current #:: acc
  }

  final override def equals(other: Any): Boolean = other match {
    case that: Timeline[E] =>
      if (this.isEmpty && that.isEmpty) true
      else if (this.isEmpty || that.isEmpty) false
      else {
        val NonEmpty(n1Head, n1Tail) = this
        val NonEmpty(n2Head, n2Tail) = that
        n1Head.unsafeRun() == n2Head.unsafeRun() && n1Tail.unsafeRun() == n2Tail.unsafeRun()
      }
    case _ => false
  }

  final override def toString: String = s"Timeline($toStringContent)"

  private[this] def toStringContent: String = this match {
    case End => ""
    case NonEmpty(recentEvent, _) =>
      s"${recentEvent.unsafeRun()}, ${Console.GREEN}...${Console.RESET}"
  }

  final def zip[T](that: Timeline[T]): Timeline[(E, T)] = this match {
    case End => End
    case NonEmpty(recentEvent, followingEvents) =>
      that match {
        case End => End
        case NonEmpty(thatRecentEvent, thatFollowingEvents) =>
          (recentEvent.unsafeRun() -> thatRecentEvent.unsafeRun()) #:: (followingEvents
            .unsafeRun() zip thatFollowingEvents.unsafeRun())
      }
  }

  final def interleave[S >: E](that: Timeline[S]): Timeline[S] = this match {
    case End => that
    case NonEmpty(recentEvent, followingEvents) =>
      recentEvent.unsafeRun() #:: that.interleave(followingEvents.unsafeRun())
  }

  final def forced: List[E] = foldRight[List[E]](List.empty)(_ :: _)
}

object Timeline {
  implicit def force[A](a: => A): A = a

  final case class NonEmpty[+E] private (recentEvent: IO[E], followingEvents: IO[Timeline[E]])
    extends Timeline[E]

  object NonEmpty {
    def apply[E](recentEvent: IO[E], followingEvents: IO[Timeline[E]]): Timeline[E] =
      new NonEmpty[E](recentEvent.memoized, followingEvents.memoized)
  }

  case object End extends Timeline[Nothing]

  final implicit class TimelineOps[E](timeline: => Timeline[E]) {
    def #::[S >: E](input: => S): Timeline[S] = NonEmpty(IO.pure(input), IO.pure(timeline))
    def #:::[S >: E](that: Timeline[S]): Timeline[S] =
      that.foldRight[Timeline[S]](timeline)(_ #:: _)
  }

  final def apply[E](recentEvent: => E, followingEvents: IO[E]*): Timeline[E] =
    recentEvent #:: followingEvents.foldRight[Timeline[E]](End)(_.unsafeRun() #:: _)

  final def apply[E](first: => E): Timeline[E] = first #:: End

  final def apply[E](first: => E, second: => E): Timeline[E] = first #:: apply(second)

  final def apply[E](first: => E, second: => E, third: => E): Timeline[E] =
    first #:: apply(second, third)

  final def apply[E](first: => E, second: => E, third: => E, fourth: => E): Timeline[E] =
    first #:: apply(second, third, fourth)

  final def apply[E](
    first: => E,
    second: => E,
    third: => E,
    fourth: => E,
    fifth: => E): Timeline[E] = first #:: apply(second, third, fourth, fifth)

  final def apply[E](
    first: => E,
    second: => E,
    third: => E,
    fourth: => E,
    fifth: => E,
    sixth: => E): Timeline[E] = first #:: apply(second, third, fourth, fifth, sixth)

  final def apply[E](
    first: => E,
    second: => E,
    third: => E,
    fourth: => E,
    fifth: => E,
    sixth: => E,
    seventh: => E): Timeline[E] = first #:: apply(second, third, fourth, fifth, sixth, seventh)

  final def apply[E](
    first: => E,
    second: => E,
    third: => E,
    fourth: => E,
    fifth: => E,
    sixth: => E,
    seventh: => E,
    eighth: => E): Timeline[E] =
    first #:: apply(second, third, fourth, fifth, sixth, seventh, eighth)

  final def apply[E](
    first: => E,
    second: => E,
    third: => E,
    fourth: => E,
    fifth: => E,
    sixth: => E,
    seventh: => E,
    eighth: => E,
    ninth: => E): Timeline[E] =
    first #:: apply(second, third, fourth, fifth, sixth, seventh, eighth, ninth)

  final def apply[E](
    first: => E,
    second: => E,
    third: => E,
    fourth: => E,
    fifth: => E,
    sixth: => E,
    seventh: => E,
    eighth: => E,
    ninth: => E,
    tenth: => E,
    followingEvents: IO[E]*): Timeline[E] =
    first #:: apply(second, third, fourth, fifth, sixth, seventh, eighth, ninth, tenth) #::: followingEvents
      .foldRight[Timeline[E]](End)(_.unsafeRun() #:: _)

  implicit def arbitrary[T](implicit arbitrary: Arbitrary[IO[T]]): Arbitrary[Timeline[T]] =
    Arbitrary(gen[T])

  def gen[T](implicit arbitrary: Arbitrary[IO[T]]): Gen[Timeline[T]] =
    Gen.containerOf[LazyList, IO[T]](arbitrary.arbitrary).map { lazyList =>
      if (lazyList.isEmpty) End
      else Timeline(lazyList.head.unsafeRun(), lazyList.tail: _*)
    }

  def genNonEmpty[T](implicit arbitrary: Arbitrary[IO[T]]): Gen[Timeline[T]] =
    Gen.nonEmptyContainerOf[LazyList, IO[T]](arbitrary.arbitrary).map { lazyList =>
      if (lazyList.isEmpty) sys.error("should not happen")
      else Timeline(lazyList.head.unsafeRun(), lazyList.tail: _*)
    }
}
