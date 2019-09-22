package collections

import org.scalacheck.{ Arbitrary, Gen }

sealed abstract class TimelineTest extends TestStyle {
  abstract class Environment {
    private[this] var eventsOccurred: Int = 0

    def resetEventsOccurredCounter(): Unit = eventsOccurred = 0

    def eventsOccurredShouldBe(expected: Int): Unit = eventsOccurredShouldBe(expected, expected)

    def eventsOccurredShouldBe(whenMemoized: Int, whenNotMemoized: Int): Unit = {
      val expected: Int =
        if (IO.isMemoizationEnabled) whenMemoized
        else whenNotMemoized

      eventsOccurred shouldBe expected
    }

    def eventsOccurredShouldNotBe(expected: Int): Unit = eventsOccurredShouldNotBe(expected, expected)

    def eventsOccurredShouldNotBe(whenMemoized: Int, whenNotMemoized: Int): Unit = {
      val expected: Int =
        if (IO.isMemoizationEnabled) whenMemoized
        else whenNotMemoized

      eventsOccurred should not be expected
    }

    def sideEffect[E](event: E): E = {
      eventsOccurred += 1
      event
    }

    def testHead[E](timeline: Timeline[E]): Unit = {
      eventsOccurredShouldBe(0)

      timeline.head
      timeline.head
      eventsOccurredShouldBe(1, 2)

      timeline.tail.head
      timeline.tail.head
      eventsOccurredShouldBe(2, 4)

      timeline.tail.tail.head
      timeline.tail.tail.head
      eventsOccurredShouldBe(3, 6)
    }

    def zeroOneTwo: Timeline[Int] = Timeline(sideEffect(0), sideEffect(1), sideEffect(2))

    val gen: Gen[IO[Int]] = Arbitrary.arbitrary[Int].map(n => IO.pure(sideEffect(n)))

    implicit val arbitrary: Arbitrary[IO[Int]] = Arbitrary(gen)
  }

  test("init") {
    new Environment {
      val timeline: Timeline[Int] = Timeline.NonEmpty(IO.pure(sideEffect(0)), IO.pure(Timeline.End))

      eventsOccurredShouldBe(0)

      val list: List[Int] = timeline.forced

      eventsOccurredShouldBe(1)
      list shouldBe List(0)
    }

    new Environment {
      testHead {
        Timeline.NonEmpty(
          IO.pure(sideEffect(0)),
          IO.pure(
            Timeline.NonEmpty(
              IO.pure(sideEffect(1)),
              IO.pure(Timeline.NonEmpty(IO.pure(sideEffect(2)), IO.pure(Timeline.End))))))
      }
    }

    new Environment {
      testHead(zeroOneTwo)
    }

    new Environment {
      testHead {
        sideEffect(0) #:: sideEffect(1) #:: sideEffect(2) #:: Timeline.End
      }
    }

    new Environment {
      def ascending(seed: Int): Timeline[Int] = sideEffect(seed) #:: ascending(seed + 1)

      val ints: Timeline[Int] = ascending(0)

      eventsOccurredShouldBe(0)
    }
  }

  test("addMany") {
    new Environment {
      val left: Timeline[Int] = zeroOneTwo
      val right: Timeline[Int] = Timeline(sideEffect(3), sideEffect(4), sideEffect(5))
      val combined: Timeline[Int] = left #::: right

      eventsOccurredShouldBe(0)

      val expected: List[Int] = List(0, 1, 2, 3, 4, 5)

      combined.forced shouldBe expected
    }

    new Environment {
      def ascending(seed: Int): Timeline[Int] = Timeline(sideEffect(seed)) #::: ascending(seed + 1)

      val ints: Timeline[Int] = ascending(0)

      eventsOccurredShouldBe(0)
    }
  }

  test("take") {
    new Environment {
      val timeline: Timeline[Int] = zeroOneTwo
      val t: Timeline[Int] = timeline.take(-1)
      val t0: Timeline[Int] = timeline.take(0)
      val t1: Timeline[Int] = timeline.take(1)
      val t2: Timeline[Int] = timeline.take(2)
      val t3: Timeline[Int] = timeline.take(3)
      val t4: Timeline[Int] = timeline.take(4)
      eventsOccurredShouldBe(0)

      t.forced shouldBe List.empty
      t0.forced shouldBe List.empty
      eventsOccurredShouldBe(0)

      t1.forced shouldBe List(0)
      eventsOccurredShouldBe(1)

      t2.forced shouldBe List(0, 1)
      eventsOccurredShouldBe(2, 3)

      t3.forced shouldBe List(0, 1, 2)
      eventsOccurredShouldBe(3, 6)

      t4.forced shouldBe List(0, 1, 2)
      eventsOccurredShouldBe(3, 9)

      timeline.forced shouldBe List(0, 1, 2)
      eventsOccurredShouldBe(3, 12)
    }
  }

  test("reversed") {
    new Environment {
      val timeline: Timeline[Int] = zeroOneTwo
      val reversed: Timeline[Int] = timeline.reversed
      eventsOccurredShouldBe(0)

      val list: List[Int] = reversed.forced
      eventsOccurredShouldBe(3)
      list shouldBe List(2, 1, 0)
    }

    new Environment {
      def ascending(seed: Int): Timeline[Int] = Timeline(sideEffect(seed)) #::: ascending(seed + 1)

      val ints: Timeline[Int] = ascending(0)
      val reversed: Timeline[Int] = ints.take(3).reversed

      eventsOccurredShouldBe(0)
      reversed.forced shouldBe List(2, 1, 0)
    }
  }

  test("apply") {
    new Environment {
      val timeline: Timeline[String] = zeroOneTwo.map(_.toString)

      timeline(-1) shouldBe None
      eventsOccurredShouldBe(0)

      (0 until timeline.size) foreach { index =>
        timeline(index) shouldBe Some(index.toString)
        eventsOccurredShouldBe(index + 1)
      }

      timeline(3) shouldBe None
      eventsOccurredShouldBe(3)
    }
  }

  test("toString") {
    new Environment {
      val timeline: Timeline[Int] = zeroOneTwo
      eventsOccurredShouldBe(0)

      val tail: String = Console.GREEN + "..." + Console.RESET

      timeline.toString shouldBe s"Timeline(0, $tail)"
      eventsOccurredShouldBe(1)

      timeline.tail.toString shouldBe s"Timeline(1, $tail)"
      eventsOccurredShouldBe(2)
    }
  }

  test("equality") {
    new Environment {
      val timeline: Timeline[Int] = zeroOneTwo

      timeline should not be Timeline.End
      eventsOccurredShouldBe(0)

      Timeline.End should not be timeline
      eventsOccurredShouldBe(0)
    }

    new Environment {
      val a: Timeline[Int] = Timeline(0, 1)
      val b: Timeline[Int] = Timeline(0)

      a should not be b
      b should not be a
    }

    new Environment {
      val a: Timeline[Int] = zeroOneTwo
      val b: Timeline[Int] = zeroOneTwo

      eventsOccurredShouldBe(0)
      a shouldBe b
      eventsOccurredShouldBe(6)
    }

    new Environment {
      forAll { timeline: Timeline[Int] =>
        eventsOccurredShouldBe(0)

        val a = timeline
        val b = timeline.map(identity)

        eventsOccurredShouldBe(0)
        a shouldBe b

        whenever(timeline.nonEmpty) {
          eventsOccurredShouldNotBe(0)
        }

        resetEventsOccurredCounter()
      }
    }
  }

  test("zip") {
    new Environment {
      val a: Timeline[Int] = Timeline.End
      val b: Timeline[String] = Timeline.End
      a zip b shouldBe Timeline.End
    }

    new Environment {
      val timeline: Timeline[(Int, String)] = zeroOneTwo.zip(zeroOneTwo.map(_.toString))
      eventsOccurredShouldBe(0)

      timeline.take(1).forced shouldBe List(0 -> "0")
      eventsOccurredShouldBe(2)

      timeline.take(2).forced shouldBe List(0 -> "0", 1 -> "1")
      eventsOccurredShouldBe(4, 6)

      timeline.forced shouldBe List(0 -> "0", 1 -> "1", 2 -> "2")
      eventsOccurredShouldBe(6, 12)
    }

    new Environment {
      val a: Timeline[Int] = Timeline(sideEffect(0), sideEffect(1))
      val b: Timeline[String] = Timeline(sideEffect("dev"), sideEffect("inside"), sideEffect("you"))
      val timeline: Timeline[(Int, String)] = a zip b

      eventsOccurredShouldBe(0)
      timeline shouldBe Timeline(0 -> "dev", 1 -> "inside")
      timeline.forced shouldBe List(0 -> "dev", 1 -> "inside")
    }

    new Environment {
      val a: Timeline[Int] = zeroOneTwo
      val b: Timeline[String] = Timeline(sideEffect("dev"), sideEffect("inside"))
      val timeline: Timeline[(Int, String)] = a zip b

      eventsOccurredShouldBe(0)
      timeline shouldBe Timeline(0 -> "dev", 1 -> "inside")
      timeline.forced shouldBe List(0 -> "dev", 1 -> "inside")
    }
  }

  test("interleave") {
    new Environment {
      val a: Timeline[Int] = Timeline.End
      val b: Timeline[Int] = Timeline.End
      a interleave b shouldBe Timeline.End
    }

    new Environment {
      val a: Timeline[Int] = zeroOneTwo
      val b: Timeline[Int] = zeroOneTwo.map(_ + 10)
      val timeline: Timeline[Int] = a interleave b
      eventsOccurredShouldBe(0)
      timeline shouldBe Timeline(0, 10, 1, 11, 2, 12)
    }

    new Environment {
      val a: Timeline[Int] = Timeline(sideEffect(0), sideEffect(1), sideEffect(2), sideEffect(3))
      val b: Timeline[Int] = Timeline(10)
      val timeline: Timeline[Int] = a interleave b
      eventsOccurredShouldBe(0)
      timeline shouldBe Timeline(0, 10, 1, 2, 3)
    }

    new Environment {
      val a: Timeline[Int] = Timeline(sideEffect(0))
      val b: Timeline[Int] = Timeline(sideEffect(10), sideEffect(11), sideEffect(12), sideEffect(13))
      val timeline: Timeline[Int] = a interleave b
      eventsOccurredShouldBe(0)
      timeline shouldBe Timeline(0, 10, 11, 12, 13)
    }
  }

  test("flatten") {
    new Environment {
      forAll { inner: Timeline[Int] =>
        val outer: Timeline[Timeline[Int]] = Timeline(inner, inner, inner)
        outer.flatten shouldBe outer.flatMap(e => e)
      }
    }
  }
}

final class TimelineTestWithMemoization extends TimelineTest {
  IO.enableMemoization()
}

final class TimelineTestWithoutMemoization extends TimelineTest {
  IO.disableMemoization()
}
