package stdlib

import scala.collection.Map

object StandardLibraryMapJoin {
  final implicit class DSL[K, V](private val self: Map[K, V]) {
    def join[T](that: Map[K, T]): Join[K, V, T] = new Join(self, that)

    class Join[Y, Z, T](self: Map[Y, Z], that: Map[Y, T]) {
      def inner: Map[Y, (Z, T)] = inner(t2)

      def inner[X](factory: (Z, T) => X): Map[Y, X] = self.foldLeft[Map[Y, X]](Map.empty) {
        case (acc, (thisKey, thisValue)) =>
          that
            .get(thisKey)
            .map { thatValue =>
              acc.+(thisKey -> factory(thisValue, thatValue))
            }
            .getOrElse(acc)
      }

      def leftOuter: Map[Y, (Z, Option[T])] = leftOuter(t2)

      def leftOuter[X](factory: (Z, Option[T]) => X): Map[Y, X] =
        self.foldLeft[Map[Y, X]](Map.empty) {
          case (acc, (thisKey, thisValue)) =>
            acc.+(thisKey -> factory(thisValue, that.get(thisKey)))
        }

      def leftOnly: Map[Y, (Z, Option[T])] = leftOnly(t2)

      def leftOnly[X](factory: (Z, Option[T]) => X): Map[Y, X] =
        self.foldLeft[Map[Y, X]](Map.empty) {
          case (acc, (thisKey, thisValue)) =>
            if (that.get(thisKey).nonEmpty) acc
            else acc.+(thisKey -> factory(thisValue, None))
        }

      def rightOuter: Map[Y, (Option[Z], T)] = rightOuter(t2)

      def rightOuter[X](factory: (Option[Z], T) => X): Map[Y, X] =
        that.foldLeft[Map[Y, X]](Map.empty) {
          case (acc, (thatKey, thatValue)) =>
            self
              .get(thatKey)
              .map { thisValue =>
                acc.+(thatKey -> factory(Some(thisValue), thatValue))
              }
              .getOrElse {
                acc.+(thatKey -> factory(None, thatValue))
              }
        }

      def rightOnly: Map[Y, (Option[Z], T)] = rightOnly(t2)

      def rightOnly[X](factory: (Option[Z], T) => X): Map[Y, X] =
        that.foldLeft[Map[Y, X]](Map.empty) {
          case (acc, (thatKey, thatValue)) =>
            self.get(thatKey).map(_ => acc).getOrElse {
              acc.+(thatKey -> factory(None, thatValue))
            }
        }

      def fullOuter: Map[Y, (Option[Z], Option[T])] = fullOuter(t2)

      def fullOuter[X](factory: (Option[Z], Option[T]) => X): Map[Y, X] = {
        val left = self.foldLeft[Map[Y, X]](Map.empty) {
          case (acc, (thisKey, thisValue)) =>
            that
              .get(thisKey)
              .map { thatValue =>
                acc.+(thisKey -> factory(Some(thisValue), Some(thatValue)))
              }
              .getOrElse {
                acc.+(thisKey -> factory(Some(thisValue), None))
              }
        }

        that.foldLeft[Map[Y, X]](left) {
          case (acc, (thatKey, thatValue)) =>
            self.get(thatKey).map(_ => acc).getOrElse {
              acc.+(thatKey -> factory(None, Some(thatValue)))
            }
        }
      }

      def outer: Map[Y, (Option[Z], Option[T])] = outer(t2)

      def outer[X](factory: (Option[Z], Option[T]) => X): Map[Y, X] = {
        val left = self.foldLeft[Map[Y, X]](Map.empty) {
          case (acc, (thisKey, thisValue)) =>
            that.get(thisKey).map(_ => acc).getOrElse {
              acc.+(thisKey -> factory(Some(thisValue), None))
            }
        }

        that.foldLeft[Map[Y, X]](left) {
          case (acc, (thatKey, thatValue)) =>
            self.get(thatKey).map(_ => acc).getOrElse {
              acc.+(thatKey -> factory(None, Some(thatValue)))
            }
        }
      }
    }
  }

  private def t2[F, S]: (F, S) => (F, S) = Tuple2.apply
}

object StandardLibraryMapJoinApp extends App {
  import StandardLibraryMapJoin.DSL

  type PersonId = Int
  type PersonName = String
  type CarBrand = String

  val people: Map[PersonId, PersonName] = Map(1 -> "alfa", 3 -> "charlie", 4 -> "delta")
  val cars: Map[PersonId, CarBrand] = Map(1 -> "audi", 2 -> "bmw", 3 -> "cadillac")

  println("–" * 50)

  println(people.join(cars).fullOuter.mkString("\n"))

  println("–" * 50)
}
