package chapter05

import cats.data.OptionT
import cats.instances.list._     // for monad
import cats.syntax.applicative._ // for pure

object OptionTExample extends App {

  type ListOption[A] = OptionT[List, A]

  val r1: ListOption[Int] = OptionT(List(Option(10)))
  val r2: ListOption[Int] = 42.pure[ListOption]

  val r3a = r1.flatMap { (x: Int) =>
    r2.map { (y: Int) =>
      x * y
    }
  }

  val r3b = for {
    x <- r1
    y <- r2
  } yield x * y

  println("R3a: ", r3a)
  println("R3b: ", r3b)

}
