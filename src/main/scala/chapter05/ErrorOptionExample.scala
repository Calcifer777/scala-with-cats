package chapter05

import cats.data.OptionT
import cats.instances.either._
import cats.syntax.applicative._

object ErrorOptionExample extends App {

  type ErrorOr[A]       = Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  val a = 3.pure[ErrorOrOption]
  val b = 7.pure[ErrorOrOption]

  val rFor = for {
    x <- a
    y <- b
  } yield x - y

  val rFlat = a.flatMap(x => b.map(y => x + y))
  println(rFlat.value) // Either[Option[Int]]

}
