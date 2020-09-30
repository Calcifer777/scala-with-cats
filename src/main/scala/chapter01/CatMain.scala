package chapter01

import cats.implicits._

object CatMain extends App {

  val cat1 = Cat("Garfield", 38, "orange and black")
  cat1.show

  val cat2 = Cat("Heathcliff", 33, "orange and black")

  println(cat1.eqv(cat2))

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  println(optionCat1.eqv(optionCat2))
}
