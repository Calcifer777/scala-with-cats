package chapter02

import cats.{ Monoid, Semigroup }
import cats.instances.int._    // for Monoid[Int]
import cats.instances.option._ // for Monoid[Option]
import cats.syntax.semigroup._ // for |+|

object SuperAdder {

  def add[A: Monoid](items: List[A]): A =
    items.foldRight(Monoid[A].empty)(_ |+| _)

}
