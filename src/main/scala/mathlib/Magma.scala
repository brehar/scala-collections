package mathlib

trait Magma[A] extends AlgebraicStructure[A] {
  def operation: ClosedBinaryOperation[A]

  protected implicit def arbitrary: Arbitrary[A]

  final protected implicit def self: Magma[A] = this

  def laws: Set[Law] = Set(closure(operation))
}

object Magma extends Summoner[Magma]
