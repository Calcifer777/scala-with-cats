package chapter01

import cats.{ Eq, Show }
import cats.instances.int._
import cats.instances.string._
import cats.syntax.eq._ // for ===

final case class Cat(name: String, age: Int, color: String)

object Cat {

  implicit class PrintableCatOps[Cat](c: Cat) {
    def format(implicit p: Printable[Cat]): String = p.format(c)
    def print(implicit p: Printable[Cat]): Unit    = println(format(p))
  }

  implicit val catShow: Show[Cat] =
    Show.show((c: Cat) => f"${c.name} is a ${c.age} year-old ${c.color} cat.")

  implicit val catEq: Eq[Cat] =
    Eq.instance[Cat] { (cat1, cat2) =>
      (cat1.name === cat2.name) &&
      (cat1.age === cat2.age) &&
      (cat1.color === cat2.color)
    }
}
