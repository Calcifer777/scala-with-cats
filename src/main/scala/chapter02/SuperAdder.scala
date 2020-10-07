package chapter02

import cats.{ Monoid => CatsMonoid, Semigroup => CatsSemigroup}
import cats.instances.int._    // for Monoid[Int]
import cats.instances.option._ // for Monoid[Option]
import cats.syntax.semigroup._ // for |+|

object SuperAdder {

  def add[A: CatsMonoid](items: List[A]): A =
    items.foldRight(CatsMonoid[A].empty)(_ |+| _)

}
