package collections

sealed abstract class Timeline[+E] {
  import Timeline._

  final def head: Option[E] = this match {
    case End => None
    case NonEmpty(recentEvent, _) => Some(recentEvent.unsafeRun())
  }

  final def tail: Timeline[E] = this match {
    case End => End
    case NonEmpty(_, followingEvents) => followingEvents.unsafeRun()
  }

  final def foldRight[R](seed: => R)(function: (=> E, => R) => R): R = this match {
    case End => seed
    case NonEmpty(recentEvent, followingEvents) =>
      lazy val followingResult = followingEvents.unsafeRun().foldRight(seed)(function)
      function(recentEvent.unsafeRun(), followingResult)
  }

  @scala.annotation.tailrec
  final def foreach[R](function: E => R): Unit = this match {
    case End =>
    case NonEmpty(recentEvent, followingEvents) =>
      function(recentEvent.unsafeRun())
      followingEvents.unsafeRun().foreach(function)
  }

  final def map[R](function: E => R): Timeline[R] = foldRight[Timeline[R]](End)(function(_) #:: _)

  final def flatMap[R](function: (=> E) => Timeline[R]): Timeline[R] = foldRight[Timeline[R]](End) {
    (current, acc) =>
      function(current).foldRight(acc)(_ #:: _)
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

  final def reversed: Timeline[E] = {
    @scala.annotation.tailrec
    def loop(timeline: Timeline[E], acc: Timeline[E]): Timeline[E] = timeline match {
      case End => acc
      case NonEmpty(recentEvent, followingEvents) =>
        loop(followingEvents.unsafeRun(), recentEvent.unsafeRun() #:: acc)
    }

    loop(this, End)
  }

  final def zip[T](that: Timeline[T]): Timeline[(E, T)] = this match {
    case End => End
    case NonEmpty(recentEvent, followingEvents) =>
      that match {
        case End => End
        case NonEmpty(thatRecentEvent, thatFollowingEvents) =>
          lazy val head = recentEvent.unsafeRun() -> thatRecentEvent.unsafeRun()
          lazy val tail = followingEvents.unsafeRun() zip thatFollowingEvents.unsafeRun()
          head #:: tail
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
  final case class NonEmpty[+E](recentEvent: IO[E], followingEvents: IO[Timeline[E]])
    extends Timeline[E]

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
}
