package chapter04

import scala.language.higherKinds

trait Monad[F[_]] {
  def pure[A](value: A): F[A]
  def flatmap[A, B](value: F[A])(func: A => F[B]): F[B]
  def map[A, B](value: F[A])(func: A => B): F[B] =
    flatmap(value)(func andThen pure)
}

object Monad {

  def pure[A, F[_]](value: A)(implicit m: Monad[F]): F[A] =
    m.pure(value)

  def flatmap[A, F[_], B](
      value: F[A]
  )(func: A => F[B])(implicit m: Monad[F]): F[B] =
    m.flatmap(value)(func)

  def leftIdentity[A, F[_], B](a: A, func: A => F[B])(
      implicit m: Monad[F]
  ): Boolean =
    flatmap(pure(a)(m))(func) == func(a)

  def rightIdentity[A, F[_]](
      a: F[A]
  )(implicit m: Monad[F]): Boolean =
    flatmap(a)(m.pure) == a

  def associativity[A, B, C, F[_]](
      a: F[A],
      f: A => F[B],
      g: B => F[C]
  )(implicit m: Monad[F]): Boolean =
    flatmap(flatmap(a)(f))(g) == flatmap(a)((va: A) => flatmap(f(va))(g))

  type Id[A] = A

  implicit val idMonad: Monad[Id] = new Monad[Id] {
    def flatmap[A, B](value: Id[A])(func: A => Id[B]): Id[B] =
      func(value)
    def pure[A](value: A): Id[A] = value
  }

}

object Examples {

  import cats.Monad
  import cats.Eval

  def foldRightUnsafe[A, B](l: List[A], acc: B)(
      f: (A, B) => B
  ): B =
    l match {
      case head :: tail => f(head, foldRightUnsafe(tail, acc)(f))
      case Nil          => acc
    }

  def foldRightEval[A, B](l: List[A], acc: Eval[B])(
      f: (A, Eval[B]) => Eval[B]
  ): Eval[B] =
    l match {
      case head :: tail =>
        Eval.defer(f(head, foldRightUnsafe(tail, acc)(f)))
      case Nil => acc
    }

  /** Almost safe */
  def foldRightSafe[A, B](l: List[A], acc: B)(
      f: (A, B) => B
  ): B =
    foldRightEval(l, Eval.now(acc)) { (a: A, evB: Eval[B]) =>
      evB.map((b: B) => f(a, b))
    }.value

}
