package mathlib

trait AbelianMonoid[A] extends Monoid[A] with Abelian[A]

object AbelianMonoid extends Summoner[AbelianMonoid]
