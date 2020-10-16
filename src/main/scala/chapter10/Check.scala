package chapter10

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated._
import cats.syntax.apply._
import cats.syntax.semigroup._


/**
 * A Predicate that transforms its input with A -> B
 * E.g. toString
  */
sealed trait Check[E, A, B] {

	import Check._

	def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B]

	def map[C](f: B => C): Check[E, A, C] =
		Map[E, A, B, C](this, f)

  def flatMap[C](f: B => Check[E, A, C]) =
    FlatMap[E, A, B, C](this, f)

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
    AndThen[E, A, B, C](this, that)

}

object Check {

	final case class Map[E, A, B, C](
			check: Check[E, A, B],
			func: B => C) extends Check[E, A, C] {
		def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
			check(in).map(func)
	}

  final case class FlatMap[E, A, B, C](
      check: Check[E, A, B],
      func: B => Check[E, A, C]) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(_.flatMap(b => func(b)(a).toEither))
  }

  final case class AndThen[E, A, B, C](
    check1: Check[E, A, B],
    check2: Check[E, B, C]
  ) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check1(a).withEither(_.flatMap(b => check2(b).toEither))
  }

  final case class Pure[E, A, B](
    func: A => Validated[E, B]
  ) extends Check[E, A, B] {
    def apply(a: A) (implicit s: Semigroup[E]): Validated[E, B] =
      func(a)
  }

  final case class PurePredicate[E, A](
    pred: Predicate[E, A]
  ) extends Check[E, A, A] {
    def apply(a: A) (implicit s: Semigroup[E]): Validated[E, A] =
      pred(a)
  }

  def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
    PurePredicate(pred)

  def apply[E, A, B](func: A => Validated[E, B]): Check[E, A, B] =
    Pure(func)
}


