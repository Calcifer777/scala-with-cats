package chapter05

import cats.data.EitherT
import cats.syntax.applicative._

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Autobots extends App {

  type Response[A] = EitherT[Future, String, A]

  def getPowerLevel(autobot: String): Response[Int] = {
    Thread.sleep(200)
    powerLevels.get(autobot) match {
      case None =>
        EitherT.left(Future(s"${autobot} unreachable"))
      case Some(v) => EitherT.right(Future(v))
    }
  }

  val powerLevels = Map(
    "Jazz"      -> 6,
    "BumbleBee" -> 7,
    "Hot Rod"   -> 10
  )

  val p = getPowerLevel("Jazz").value: Future[
    Either[String, Int]
  ]
  println(Await.result(p, 1.second).toOption.get)

  def canSpecialMove(
      ally1: String,
      ally2: String
  ): Response[Boolean] =
    for {
      p1 <- getPowerLevel(ally1)
      p2 <- getPowerLevel(ally2)
    } yield p1 + p2 > 15

  def tacticalReport(ally1: String, ally2: String): String = {
    val r = canSpecialMove(ally1, ally2).value
    Await.result(r, 1.second) match {
      case Left(msg) => s"Comms error: $msg"
      case Right(true) =>
        s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) => s"$ally1 and $ally2 need a recharge"
    }
  }

  println(tacticalReport("Optimus Prime", "BumbleBee"))
  println(tacticalReport("Jazz", "BumbleBee"))
  println(tacticalReport("Jazz", "Hot Rod"))

}
