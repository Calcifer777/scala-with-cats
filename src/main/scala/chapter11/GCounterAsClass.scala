package chapter11

import cats.syntax.semigroup._
import cats.kernel.CommutativeMonoid
import BoundedSemiLattice._

final case class GCounterAsClass[A](counters: Map[String, A]) {

  def increment(machine: String, amount: A)(implicit cm: CommutativeMonoid[A]): GCounterAsClass[A] = {
    val value = amount |+| counters.getOrElse(machine, cm.empty)
    GCounterAsClass(counters + (machine -> value))
  }

  def merge(that: GCounterAsClass[A])(implicit bsl: BoundedSemiLattice[A]): GCounterAsClass[A] =
    new GCounterAsClass(this.counters |+| that.counters)

  def total(implicit cm: CommutativeMonoid[A]): A = counters.values.fold(cm.empty)(cm.combine)
}

