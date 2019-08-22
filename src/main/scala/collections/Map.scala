package collections

final class Map[K, +V] private (val keys: Set[K], valueOf: K => Option[V])
  extends (K => Option[V])
  with FoldableFactory2[K, V, Map] {
  protected def factory: Factory2[Map] = Map

  def apply(key: K): Option[V] = valueOf(key)

  lazy val values: Set[V] = keys.map(unsafeValueOf)

  private[this] def unsafeValueOf(key: K): V = valueOf(key).get

  def fold[R](seed: R)(function: (R, (K, V)) => R): R = keys.fold(seed) { (acc, currentKey) =>
    function(acc, currentKey -> unsafeValueOf(currentKey))
  }

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
}

object Map extends Factory2[Map] {
  final def empty[K, V]: Map[K, V] = apply(Set.empty, _ => None)

  private def apply[K, V](keys: Set[K], valueOf: K => Option[V]): Map[K, V] = new Map(keys, valueOf)
}
