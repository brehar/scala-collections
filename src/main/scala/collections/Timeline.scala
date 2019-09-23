package collections

import org.scalacheck.{ Arbitrary, Gen }

final class Timeline[+E] private (private val data: IO[Timeline.Data[E]])
  extends (Int => Option[E]) {
  def apply(index: Int): Option[E] = data.unsafeRun().apply(index)

  def head: Option[E] = data.unsafeRun().head

  def tail: Timeline[E] = Timeline(data.map(_.tail))

  def isEmpty: Boolean = this == Timeline.End

  def nonEmpty: Boolean = !isEmpty

  def foldLeft[R](seed: R)(function: (R, => E) => R): R = data.unsafeRun().foldLeft(seed)(function)

  def foldRight[R](seed: => R)(function: (=> E, => R) => R): R =
    data.unsafeRun().foldRight(seed)(function)

  def size: Int = data.unsafeRun().size

  def take(amount: Int): Timeline[E] = Timeline(data.map(_.take(amount)))

  def reversed: Timeline[E] = Timeline(data.map(_.reversed))

  def foreach[R](function: E => R): Unit = data.unsafeRun().foreach(function)

  def filter(predicate: E => Boolean): Timeline[E] = Timeline(data.map(_.filter(predicate)))

  def withFilter(predicate: E => Boolean): Timeline[E] = filter(predicate)

  def takeWhile(predicate: E => Boolean): Timeline[E] = Timeline(data.map(_.takeWhile(predicate)))

  def map[R](function: E => R): Timeline[R] = Timeline(data.map(_.map(function)))

  def flatMap[R](function: (=> E) => Timeline[R]): Timeline[R] =
    Timeline(data.map(_.flatMap(e => function(e).data.unsafeRun())))

  def flatten[R](implicit view: (=> E) => Timeline[R]): Timeline[R] =
    Timeline(data.map(_.flatMap(e => view(e).data.unsafeRun())))

  override def equals(other: Any): Boolean = other match {
    case that: Timeline[E] => this.data.unsafeRun() == that.data.unsafeRun()
    case _ => false
  }

  override def toString: String = s"Timeline($toStringContent)"

  private[this] def toStringContent: String = this match {
    case Timeline.End => ""
    case Timeline.NonEmpty(recentEvent, _) =>
      s"${recentEvent.unsafeRun()}, ${Console.GREEN}...${Console.RESET}"
  }

  def zip[T](that: Timeline[T]): Timeline[(E, T)] = Timeline(data.map(_.zip(that.data.unsafeRun())))

  def interleave[S >: E](that: Timeline[S]): Timeline[S] =
    Timeline(data.map(_.interleave(that.data.unsafeRun())))

  def forced: List[E] = data.unsafeRun().forced
}

object Timeline {
  implicit def force[A](a: => A): A = a

  final private def apply[E](data: IO[Data[E]]): Timeline[E] = new Timeline(data.memoized)

  object NonEmpty {
    def apply[E](recentEvent: IO[E], followingEvents: IO[Timeline[E]]): Timeline[E] =
      Timeline(followingEvents.map { timeline =>
        Data.NonEmpty(recentEvent, timeline.data)
      })

    def unapply[E](timeline: Timeline[E]): Option[(IO[E], IO[Timeline[E]])] =
      timeline.data.unsafeRun() match {
        case Data.End => None
        case Data.NonEmpty(recentEvent, followingEvents) =>
          Some(recentEvent -> IO.pure(Timeline(followingEvents)))
      }
  }

  final val End: Timeline[Nothing] = Timeline(IO.pure(Data.End))

  final implicit class TimelineOps[E](timeline: => Timeline[E]) {
    def #::[S >: E](input: => S): Timeline[S] = NonEmpty(IO.pure(input), IO.pure(timeline))

    def #:::[S >: E](that: Timeline[S]): Timeline[S] =
      that.foldRight[Timeline[S]](timeline)(_ #:: _)
  }

  final def apply[E](recentEvent: => E, followingEvents: IO[E]*): Timeline[E] =
    Timeline(IO.pure(Data(recentEvent, followingEvents: _*)))

  final def apply[E](first: => E): Timeline[E] = Timeline(IO.pure(Data(first)))

  final def apply[E](first: => E, second: => E): Timeline[E] =
    Timeline(IO.pure(Data(first, second)))

  final def apply[E](first: => E, second: => E, third: => E): Timeline[E] =
    Timeline(IO.pure(Data(first, second, third)))

  final def apply[E](first: => E, second: => E, third: => E, fourth: => E): Timeline[E] =
    Timeline(IO.pure(Data(first, second, third, fourth)))

  final def apply[E](
    first: => E,
    second: => E,
    third: => E,
    fourth: => E,
    fifth: => E): Timeline[E] = Timeline(IO.pure(Data(first, second, third, fourth, fifth)))

  final def apply[E](
    first: => E,
    second: => E,
    third: => E,
    fourth: => E,
    fifth: => E,
    sixth: => E): Timeline[E] =
    Timeline(IO.pure(Data(first, second, third, fourth, fifth, sixth)))

  final def apply[E](
    first: => E,
    second: => E,
    third: => E,
    fourth: => E,
    fifth: => E,
    sixth: => E,
    seventh: => E): Timeline[E] =
    Timeline(IO.pure(Data(first, second, third, fourth, fifth, sixth, seventh)))

  final def apply[E](
    first: => E,
    second: => E,
    third: => E,
    fourth: => E,
    fifth: => E,
    sixth: => E,
    seventh: => E,
    eighth: => E): Timeline[E] =
    Timeline(IO.pure(Data(first, second, third, fourth, fifth, sixth, seventh, eighth)))

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
    Timeline(IO.pure(Data(first, second, third, fourth, fifth, sixth, seventh, eighth, ninth)))

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
    Timeline(
      IO.pure(
        Data(
          first,
          second,
          third,
          fourth,
          fifth,
          sixth,
          seventh,
          eighth,
          ninth,
          tenth,
          followingEvents: _*)))

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

  private sealed abstract class Data[+E] extends (Int => Option[E]) {
    final def apply(index: Int): Option[E] = {
      @scala.annotation.tailrec
      def loop(data: Data[E], count: Int): Option[E] =
        if (index < 0) None
        else if (count == index) data.head
        else loop(data.tail, count + 1)

      loop(this, 0)
    }

    final def head: Option[E] = this match {
      case Data.End => None
      case Data.NonEmpty(recentEvent, _) => Some(recentEvent.unsafeRun())
    }

    final def tail: Data[E] = this match {
      case Data.End => Data.End
      case Data.NonEmpty(_, followingEvents) => followingEvents.unsafeRun()
    }

    final def isEmpty: Boolean = this.isInstanceOf[Data.End.type]

    final def nonEmpty: Boolean = !isEmpty

    @scala.annotation.tailrec
    final def foldLeft[R](seed: R)(function: (R, => E) => R): R = this match {
      case Data.End => seed
      case Data.NonEmpty(recentEvent, followingEvents) =>
        val currentResult = function(seed, recentEvent.unsafeRun())
        followingEvents.unsafeRun().foldLeft(currentResult)(function)
    }

    final def foldRight[R](seed: => R)(function: (=> E, => R) => R): R = this match {
      case Data.End => seed
      case Data.NonEmpty(recentEvent, followingEvents) =>
        lazy val followingResult = followingEvents.unsafeRun().foldRight(seed)(function)
        function(recentEvent.unsafeRun(), followingResult)
    }

    final def size: Int = foldLeft(0) { (acc, _) =>
      acc + 1
    }

    final def take(amount: Int): Data[E] = {
      @scala.annotation.tailrec
      def loop(data: Data[E], acc: Data[E], count: Int): Data[E] = data match {
        case Data.End => acc
        case Data.NonEmpty(recentEvent, followingEvents) =>
          if (count >= amount) acc
          else loop(followingEvents.unsafeRun(), recentEvent.unsafeRun() #:: acc, count + 1)
      }

      loop(this, Data.End, 0).reversed
    }

    final def reversed: Data[E] = foldLeft[Data[E]](Data.End) { (acc, current) =>
      current #:: acc
    }

    final def foreach[R](function: E => R): Unit = foldLeft(()) { (_, current) =>
      function(current)
    }

    final def filter(predicate: E => Boolean): Data[E] = foldRight[Data[E]](Data.End) {
      (current, acc) =>
        if (predicate(current)) current #:: acc
        else acc
    }

    final def withFilter(predicate: E => Boolean): Data[E] = filter(predicate)

    final def takeWhile(predicate: E => Boolean): Data[E] = foldRight[Data[E]](Data.End) {
      (current, acc) =>
        if (predicate(current)) current #:: acc
        else Data.End
    }

    final def map[R](function: E => R): Data[R] = foldRight[Data[R]](Data.End)(function(_) #:: _)

    final def flatMap[R](function: (=> E) => Data[R]): Data[R] = foldRight[Data[R]](Data.End) {
      (current, acc) =>
        function(current).foldRight(acc)(_ #:: _)
    }

    final def flatten[R](implicit view: (=> E) => Data[R]): Data[R] =
      foldRight[Data[R]](Data.End) { (current, acc) =>
        view(current).foldRight(acc)(_ #:: _)
      }

    final override def equals(other: Any): Boolean = other match {
      case that: Data[E] =>
        if (this.isEmpty && that.isEmpty) true
        else if (this.isEmpty || that.isEmpty) false
        else {
          val Data.NonEmpty(n1Head, n1Tail) = this
          val Data.NonEmpty(n2Head, n2Tail) = that
          n1Head.unsafeRun() == n2Head.unsafeRun() && n1Tail.unsafeRun() == n2Tail.unsafeRun()
        }
      case _ => false
    }

    final override def toString: String = s"Data($toStringContent)"

    private[this] def toStringContent: String = this match {
      case Data.End => ""
      case Data.NonEmpty(recentEvent, _) =>
        s"${recentEvent.unsafeRun()}, ${Console.GREEN}...${Console.RESET}"
    }

    final def zip[T](that: Data[T]): Data[(E, T)] = this match {
      case Data.End => Data.End
      case Data.NonEmpty(recentEvent, followingEvents) =>
        that match {
          case Data.End => Data.End
          case Data.NonEmpty(thatRecentEvent, thatFollowingEvents) =>
            (recentEvent.unsafeRun() -> thatRecentEvent.unsafeRun()) #:: (followingEvents
              .unsafeRun() zip thatFollowingEvents.unsafeRun())
        }
    }

    final def interleave[S >: E](that: Data[S]): Data[S] = this match {
      case Data.End => that
      case Data.NonEmpty(recentEvent, followingEvents) =>
        recentEvent.unsafeRun() #:: that.interleave(followingEvents.unsafeRun())
    }

    final def forced: List[E] = foldRight[List[E]](List.empty)(_ :: _)
  }

  private object Data {
    final case class NonEmpty[+E] private (recentEvent: IO[E], followingEvents: IO[Data[E]])
      extends Data[E]

    object NonEmpty {
      def apply[E](recentEvent: IO[E], followingEvents: IO[Data[E]]): Data[E] =
        new NonEmpty[E](recentEvent.memoized, followingEvents.memoized)
    }

    case object End extends Data[Nothing]

    final implicit class DataOps[E](data: => Data[E]) {
      def #::[S >: E](input: => S): Data[S] = NonEmpty(IO.pure(input), IO.pure(data))

      def #:::[S >: E](that: Data[S]): Data[S] =
        that.foldRight[Data[S]](data)(_ #:: _)
    }

    final def apply[E](recentEvent: => E, followingEvents: IO[E]*): Data[E] =
      recentEvent #:: followingEvents.foldRight[Data[E]](End)(_.unsafeRun() #:: _)

    final def apply[E](first: => E): Data[E] = first #:: End

    final def apply[E](first: => E, second: => E): Data[E] = first #:: apply(second)

    final def apply[E](first: => E, second: => E, third: => E): Data[E] =
      first #:: apply(second, third)

    final def apply[E](first: => E, second: => E, third: => E, fourth: => E): Data[E] =
      first #:: apply(second, third, fourth)

    final def apply[E](first: => E, second: => E, third: => E, fourth: => E, fifth: => E): Data[E] =
      first #:: apply(second, third, fourth, fifth)

    final def apply[E](
      first: => E,
      second: => E,
      third: => E,
      fourth: => E,
      fifth: => E,
      sixth: => E): Data[E] = first #:: apply(second, third, fourth, fifth, sixth)

    final def apply[E](
      first: => E,
      second: => E,
      third: => E,
      fourth: => E,
      fifth: => E,
      sixth: => E,
      seventh: => E): Data[E] = first #:: apply(second, third, fourth, fifth, sixth, seventh)

    final def apply[E](
      first: => E,
      second: => E,
      third: => E,
      fourth: => E,
      fifth: => E,
      sixth: => E,
      seventh: => E,
      eighth: => E): Data[E] =
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
      ninth: => E): Data[E] =
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
      followingEvents: IO[E]*): Data[E] =
      first #:: apply(second, third, fourth, fifth, sixth, seventh, eighth, ninth, tenth) #::: followingEvents
        .foldRight[Data[E]](End)(_.unsafeRun() #:: _)
  }
}
