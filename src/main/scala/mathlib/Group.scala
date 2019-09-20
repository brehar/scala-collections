package mathlib

trait Group[A] extends Monoid[A] {
  def uniqueInverseElement: A => A

  override def laws: Set[Law] =
    super.laws + invertability(operation, uniqueIdentityElement, uniqueInverseElement)
}

object Group extends Summoner[Group]
