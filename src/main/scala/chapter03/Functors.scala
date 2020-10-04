package chapter03

import scala.language.higherKinds
import cats.Invariant

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def lift[A, B](f: A => B): F[A] => F[B]
}

object Functor {
  def identityLaw[F[_], A](fa: F[A])(implicit f: Functor[F]) =
    f.map(fa)(a => a) == fa
  def composition[F[_], A, B, C](fa: F[A], g: A => B, h: B => C)(implicit f: Functor[F]) =
    f.map(fa)(g andThen h) == f.map(f.map(fa)(g))(h)
}
