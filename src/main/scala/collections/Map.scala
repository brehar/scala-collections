package collections

final class Map[K, +V] private (val keys: Set[K], valueOf: K => Option[V])
  extends (K => Option[V]) {
  def apply(key: K): Option[V] = valueOf(key)

  lazy val values: Set[V] = keys.map(unsafeValueOf)

  private[this] def unsafeValueOf(key: K): V = valueOf(key).get

  def add[S >: V](input: (K, S)): Map[K, S] = {
    val (key, value) = input

    Map(keys = keys.add(key), valueOf = {
      case `key` => Some(value)
      case k => valueOf(k)
    })
  }

  def remove(key: K): Map[K, V] =
    Map(keys = keys.remove(key), valueOf = {
      case `key` => None
      case k => valueOf(k)
    })

  def isEmpty: Boolean = keys.isEmpty

  def nonEmpty: Boolean = !isEmpty

  def isSingleton: Boolean = keys.isSingleton

  def sample: Option[(K, V)] = keys.sample.map(key => key -> unsafeValueOf(key))
}

object Map {
  final def empty[K, V]: Map[K, V] = apply(Set.empty, _ => None)

  private def apply[K, V](keys: Set[K], valueOf: K => Option[V]): Map[K, V] = new Map(keys, valueOf)

  final def apply[K, V](element: (K, V), otherElements: (K, V)*): Map[K, V] =
    otherElements.foldLeft[Map[K, V]](empty.add(element))(_ add _)
}
