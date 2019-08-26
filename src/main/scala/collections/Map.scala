package collections

final class Map[K, +V] private (val keys: Set[K], valueOf: K => Option[V], default: Option[K => V])
  extends (K => Option[V])
  with FoldableFactory2[K, V, Map] {
  protected def factory: Factory2[Map] = Map

  def apply(key: K): Option[V] = valueOf(key) orElse default.map(_ apply key)

  lazy val values: Set[V] = keys.map(unsafeValueOf)

  private[this] def unsafeValueOf(key: K): V = valueOf(key).get

  def fold[R](seed: R)(function: (R, (K, V)) => R): R = keys.fold(seed) { (acc, currentKey) =>
    function(acc, currentKey -> unsafeValueOf(currentKey))
  }

  def add[S >: V](input: (K, S)): Map[K, S] = {
    val (key, value) = input

    copy(keys = keys.add(key), valueOf = {
      case `key` => Some(value)
      case k => valueOf(k)
    })
  }

  def remove(key: K): Map[K, V] =
    copy(keys = keys.remove(key), valueOf = {
      case `key` => None
      case k => valueOf(k)
    })

  def isSubsetOf[S >: V](that: Map[K, S]): Boolean = forall {
    case (key, value) => that(key).contains(value)
  }

  def isSupersetOf[S >: V](that: Map[K, S]): Boolean = that.isSubsetOf(this)

  override def equals(other: Any): Boolean = other match {
    case that: Map[K, V] => this.isSubsetOf(that) && that.isSubsetOf(this)
    case _ => false
  }

  override def hashCode: Int = fold(42)(_ + _.hashCode())

  override def toString: String = keys.tree match {
    case Tree.Empty => "Map()"
    case Tree.NonEmpty(left, key, right) =>
      "Map(" + unsafeRendered(key) + splitByCommaSpace(left) + splitByCommaSpace(right) + ")"
  }

  private[this] def unsafeRendered(key: K): String = s"$key -> ${unsafeValueOf(key)}"

  private[this] def splitByCommaSpace(input: Tree[K]): String = input.fold("") {
    (acc, currentKey) =>
      s"$acc, ${unsafeRendered(currentKey)}"
  }

  def isEmpty: Boolean = keys.isEmpty

  def nonEmpty: Boolean = !isEmpty

  def isSingleton: Boolean = keys.isSingleton

  def sample: Option[(K, V)] = keys.sample.map(key => key -> unsafeValueOf(key))

  def withDefault[S >: V](default: K => S): Map[K, S] = copy(default = Some(default))

  def withDefaultValue[S >: V](defaultValue: => S): Map[K, S] = withDefault(_ => defaultValue)

  def getOrElseUpdated[S >: V](key: K, newValue: => S): (S, Map[K, S]) =
    valueOf(key).map(_ -> this).getOrElse {
      val value = newValue
      value -> add(key -> value)
    }

  def mapValues[N](function: V => N): Map[K, N] = map {
    case (key, value) => key -> function(value)
  }

  def filterKeys(predicate: K => Boolean): Map[K, V] = filter {
    case (key, _) => predicate(key)
  }

  def filterValues(predicate: V => Boolean): Map[K, V] = filter {
    case (_, value) => predicate(value)
  }

  private[this] def copy[S >: V](
    keys: Set[K] = keys,
    valueOf: K => Option[S] = valueOf,
    default: Option[K => S] = default): Map[K, S] = Map(keys, valueOf, default)
}

object Map extends Factory2[Map] {
  final def empty[K, V]: Map[K, V] = apply(Set.empty, _ => None, None)

  private def apply[K, V](
    keys: Set[K],
    valueOf: K => Option[V],
    default: Option[K => V]): Map[K, V] = new Map(keys, valueOf, default)

  def withKeys[E](keys: Set[E]): Source[E] = new Source(keys)

  final class Source[E](val keys: Set[E]) extends AnyVal {
    def andSomeValues[V](valueOf: PartialFunction[E, V]): Map[E, V] = andValues(valueOf.lift)

    def andValues[V](valueOf: E => Option[V]): Map[E, V] = keys.fold[Map[E, V]](Map.empty) {
      (acc, currentKey) =>
        valueOf(currentKey).map(value => acc.add(currentKey -> value)).getOrElse(acc)
    }
  }

  object PotentiallyDangerousImplicits {
    final implicit class MapExtensions[K, V](val self: Map[K, V]) extends AnyVal {
      def mapKeys[N](function: K => N): Map[N, V] = self.map {
        case (key, value) => function(key) -> value
      }

      def swapped: Map[V, K] = self.map {
        case (key, value) => value -> key
      }
    }
  }
}
