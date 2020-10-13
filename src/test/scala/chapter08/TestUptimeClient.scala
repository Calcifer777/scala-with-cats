package chapter08

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import cats.Id
import cats.syntax.applicative._

trait TestUptimeClient extends UptimeClient[Id] {
  def getUptime(hostname: String): Id[Int]
}

class TestUptimeClientImpl(hosts: Map[String, Int]) extends UptimeClient[Id] {
  def getUptime(hostname: String): Id[Int] =
    hosts.getOrElse(hostname, 0)
}
