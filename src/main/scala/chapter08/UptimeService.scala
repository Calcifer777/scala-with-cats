package chapter08

import cats.Applicative
import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.functor._  // needed for cats.Functor in getTotalUptime

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): 
      F[Int] = 
    hostnames                     // List[String]
      .traverse(client.getUptime) // F[List[Int]]
      .map(_.sum)                 // F[Int]
}
