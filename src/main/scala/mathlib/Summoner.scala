package mathlib

trait Summoner[B[_]] {
  def apply[A: B]: B[A] = implicitly[B[A]]
}
