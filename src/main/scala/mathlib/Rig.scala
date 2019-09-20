package mathlib

trait Rig[A] extends AbelianMonoid[A] {
  def that: Monoid[A]

  override def laws: Set[Law] =
    super.laws ++ that.laws + distributivity(this.operation, that.operation)
}

object Rig extends Summoner[Rig]
