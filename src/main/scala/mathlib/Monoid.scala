package mathlib

trait Monoid[A] extends Semigroup[A] {
  def uniqueIdentityElement: A

  override def laws: Set[Law] = super.laws + identity(operation, uniqueIdentityElement)
}

object Monoid extends Summoner[Monoid]
