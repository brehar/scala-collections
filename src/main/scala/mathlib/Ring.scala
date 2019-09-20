package mathlib

trait Ring[A] extends Rng[A] {
  def that: Monoid[A]
}

object Ring extends Summoner[Ring]
