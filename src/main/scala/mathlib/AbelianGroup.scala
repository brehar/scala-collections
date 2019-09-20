package mathlib

trait AbelianGroup[A] extends Group[A] with Abelian[A]

object AbelianGroup extends Summoner[AbelianGroup]
