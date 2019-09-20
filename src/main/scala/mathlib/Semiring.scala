package mathlib

trait Semiring[A] extends AbelianSemigroup[A] {
  def that: Monoid[A]

  override def laws: Set[Law] =
    super.laws ++ that.laws + distributivity(this.operation, that.operation)
}

object Semiring extends Summoner[Semiring]
