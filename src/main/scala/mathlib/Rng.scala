package mathlib

trait Rng[A] extends AbelianGroup[A] {
  def that: Semigroup[A]

  override def laws: Set[Law] =
    super.laws ++ that.laws + distributivity(this.operation, that.operation)
}

object Rng extends Summoner[Rng]
