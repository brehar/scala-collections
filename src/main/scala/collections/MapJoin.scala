package collections

object MapJoin {
  final implicit class DSL[K, V](private val self: Map[K, V]) {
    def join[T](that: Map[K, T]): Join[K, V, T] = new Join(self, that)

    class Join[Y, Z, T](self: Map[Y, Z], that: Map[Y, T]) {
      def inner: Map[Y, (Z, T)] = inner(t2)

      def inner[X](factory: (Z, T) => X): Map[Y, X] = self.fold[Map[Y, X]](Map.empty) {
        case (acc, (thisKey, thisValue)) =>
          that(thisKey)
            .map { thatValue =>
              acc.add(thisKey -> factory(thisValue, thatValue))
            }
            .getOrElse(acc)
      }

      def leftOuter: Map[Y, (Z, Option[T])] = leftOuter(t2)

      def leftOuter[X](factory: (Z, Option[T]) => X): Map[Y, X] = self.fold[Map[Y, X]](Map.empty) {
        case (acc, (thisKey, thisValue)) => acc.add(thisKey -> factory(thisValue, that(thisKey)))
      }

      def leftOnly: Map[Y, (Z, Option[T])] = leftOnly(t2)

      def leftOnly[X](factory: (Z, Option[T]) => X): Map[Y, X] = self.fold[Map[Y, X]](Map.empty) {
        case (acc, (thisKey, thisValue)) =>
          if (that(thisKey).nonEmpty) acc
          else acc.add(thisKey -> factory(thisValue, None))
      }

      def rightOuter: Map[Y, (Option[Z], T)] = rightOuter(t2)

      def rightOuter[X](factory: (Option[Z], T) => X): Map[Y, X] = that.fold[Map[Y, X]](Map.empty) {
        case (acc, (thatKey, thatValue)) =>
          self(thatKey)
            .map { thisValue =>
              acc.add(thatKey -> factory(Some(thisValue), thatValue))
            }
            .getOrElse {
              acc.add(thatKey -> factory(None, thatValue))
            }
      }

      def rightOnly: Map[Y, (Option[Z], T)] = rightOnly(t2)

      def rightOnly[X](factory: (Option[Z], T) => X): Map[Y, X] = that.fold[Map[Y, X]](Map.empty) {
        case (acc, (thatKey, thatValue)) =>
          self(thatKey).map(_ => acc).getOrElse {
            acc.add(thatKey -> factory(None, thatValue))
          }
      }

      def fullOuter: Map[Y, (Option[Z], Option[T])] = fullOuter(t2)

      def fullOuter[X](factory: (Option[Z], Option[T]) => X): Map[Y, X] = {
        val left = self.fold[Map[Y, X]](Map.empty) {
          case (acc, (thisKey, thisValue)) =>
            that(thisKey)
              .map { thatValue =>
                acc.add(thisKey -> factory(Some(thisValue), Some(thatValue)))
              }
              .getOrElse {
                acc.add(thisKey -> factory(Some(thisValue), None))
              }
        }

        that.fold[Map[Y, X]](left) {
          case (acc, (thatKey, thatValue)) =>
            self(thatKey).map(_ => acc).getOrElse {
              acc.add(thatKey -> factory(None, Some(thatValue)))
            }
        }
      }

      def outer: Map[Y, (Option[Z], Option[T])] = outer(t2)

      def outer[X](factory: (Option[Z], Option[T]) => X): Map[Y, X] = {
        val left = self.fold[Map[Y, X]](Map.empty) {
          case (acc, (thisKey, thisValue)) =>
            that(thisKey).map(_ => acc).getOrElse {
              acc.add(thisKey -> factory(Some(thisValue), None))
            }
        }

        that.fold[Map[Y, X]](left) {
          case (acc, (thatKey, thatValue)) =>
            self(thatKey).map(_ => acc).getOrElse {
              acc.add(thatKey -> factory(None, Some(thatValue)))
            }
        }
      }
    }
  }

  private def t2[F, S]: (F, S) => (F, S) = Tuple2.apply
}
