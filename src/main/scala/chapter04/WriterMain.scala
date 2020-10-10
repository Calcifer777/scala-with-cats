package chapter04

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object WriterMain extends App {

  def slowly[A](body: => A) =
    try body
    finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(f"fact ${n} ${ans}")
    ans
  }

  println("*** Logs from multithread ***")
  Await.result(
    Future.sequence(
      Vector(
        Future(factorial(4)),
        Future(factorial(4))
      )
    ),
    5.seconds
  )

  type Logged[A] = Writer[Vector[String], A]

  def factorialW(n: Int): Logged[Int] =
    for {
      ans <- if (n == 0) 1.pure[Logged]
      else {
        slowly {
          factorialW(n - 1).map(_ * n)
        }
      }
      _ <- Vector(f"fact ${n} ${ans}").tell
    } yield ans

  println("*** Logs from multithread with Writer context***")
  val v = Await.result(
    Future.sequence(
      Vector(
        Future(factorialW(4)),
        Future(factorialW(4))
      )
    ),
    5.seconds
  )
  v map (_.written) foreach (println)

}
