package chapter04

import cats.data.Reader
import cats.syntax.applicative._

final case class Db(
    usernames: Map[Int, String],
    passwords: Map[String, String]
)

object ReaderMain extends App {

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
    for {
      username  <- findUsername(userId)
      validPass <- username.map(username => checkPassword(username, password)).getOrElse(false.pure[DbReader])
    } yield validPass
  }

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acirdburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  println(checkLogin(1, "zerocool").run(db))
  // println(checkLogin(1, "zerocool")(db)) same as above
  println(checkLogin(4, "davinci").run(db))
}
