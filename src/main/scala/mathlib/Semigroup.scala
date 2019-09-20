package mathlib

trait Semigroup[A] extends Magma[A] {
  override def laws: Set[Law] = super.laws + associativity(operation)
}

object Semigroup extends Summoner[Semigroup]
