package chapter10


import cats.data.Validated
import cats._
import cats.implicits._


final case class User(username: String, email: String)

object User {

  import CheckClient._
  import Predicate._

  def createUser(username: String, email: String): Validated[Errors, User] =
    (checkUsername(username), checkEmail(email)).mapN(User.apply(_, _))

}


