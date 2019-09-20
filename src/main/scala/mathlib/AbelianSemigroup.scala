package mathlib

trait AbelianSemigroup[A] extends Semigroup[A] with Abelian[A]

object AbelianSemigroup extends Summoner[AbelianSemigroup]
