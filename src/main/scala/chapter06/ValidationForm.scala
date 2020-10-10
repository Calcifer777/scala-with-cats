package chapter06

import cats.data.Validated
import cats.syntax.either._
import cats.syntax.apply._

object ValidationForm extends App {

  type FormData  = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  case class User(name: String, age: Int)

  def getValue(field: String)(data: FormData): FailFast[String] =
    data.get(field).toRight(List(s"$field field not specified"))

  def parseInt(field: String)(data: String): FailFast[Int] = 
    Either.catchOnly[NumberFormatException](field.toInt)
          .leftMap(_ => List(s"$field must be an integer"))
    
  def nonBlank(field: String)(data: String): FailFast[String] =
    Right(data).ensure(List(s"$field cannot be blank"))(_.nonEmpty)

  def nonNegative(field: String)(data: Int): FailFast[Int] =
    Right(data).ensure(List(s"$field cannot be negative"))(_ > 0)

  def readName(data: FormData): FailFast[String] =
    getValue("name")(data).flatMap(nonBlank("name"))

  def readAge(data: FormData): FailFast[Int] =
    getValue("age")(data).
      flatMap(nonBlank("age")).
      flatMap(parseInt("age")).
      flatMap(nonNegative("age"))

  def validateForm(data: FormData): FailSlow[User] = {
    (
      Validated.fromEither(readName(data)),
      Validated.fromEither(readAge(data))
    ).mapN(User.apply)
  }

}
