package mathlib

trait Abelian[A] extends Magma[A] {
  override def laws: Set[Law] = super.laws + commutativity(operation)
}
