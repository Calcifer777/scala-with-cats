package chapter10

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated._
import cats.syntax.apply._
import cats.syntax.semigroup._
import cats.syntax.validated._


sealed trait Predicate[E, A] {

  import Predicate._

  def and(that: Predicate[E, A]): Predicate[E, A] =
    And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] =
    Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = {
    this match {
      case Pure(f) => f(a)
      case And(left, right) => (left(a), right(a)).mapN((_, _) => a)
      case Or(left, right) => left(a) match {
        case Valid(a) => Valid(a)
        case Invalid(e1) => right(a) match {
          case Valid(a) => Valid(a)
          case Invalid(e2) => Invalid(e1 |+| e2)
        }
      }
    }
  }

}

object Predicate {

  /**
   * Combines two validation functions (Pure[E, A]) with
   * a logical AND
   */
  final case class And[E, A](
    left: Predicate[E, A],
    right: Predicate[E, A]
  ) extends Predicate[E, A]

  /**
   * Combines two validation functions (Pure[E, A]) with
   * a logical OR
   */
  final case class Or[E, A](
    left: Predicate[E, A],
    right: Predicate[E, A]
  ) extends Predicate[E, A]


  /**
   * A function that validates an input of type A
   */
  final case class Pure[E, A](
    f: A => Validated[E, A]
  ) extends Predicate[E, A]

  def pure[E, A](f: A => Validated[E, A]): Predicate[E, A] = 
    Pure(f)

  def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] =
    Pure(a => if(fn(a)) a.valid else err.invalid)

}
