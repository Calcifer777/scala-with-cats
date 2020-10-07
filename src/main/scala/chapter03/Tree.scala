package chapter03

import scala.language.higherKinds

sealed trait Tree[+A]
case class Leaf[A](a: A)                   extends Tree[A]
case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

object Tree {

  def node[A](l: Tree[A], r: Tree[A]): Tree[A] =
    Node(l, r)

  def leaf[A](v: A): Tree[A] =
    Leaf(v)

  implicit val treeFunctor: Functor[Tree] =
    new Functor[Tree] {
      def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
        fa match {
          case Node(l, r) => Node(map(l)(f), map(r)(f))
          case Leaf(v)    => Leaf(f(v))
        }
      def lift[A, B](f: A => B): Tree[A] => Tree[B] =
        (t: Tree[A]) => map(t)(f)
    }

  implicit class TreeFunctorOps[A, Tree[_]](t: Tree[A]) {
    def map[B](f: A => B)(implicit functor: Functor[Tree]): Tree[B] =
      functor.map(t)(f)
  }
}

object TreeMain extends App {
  import Tree._
  // needs smart constructors for functor syntax to work
  val t = node(leaf(3), node(leaf(4), leaf(7)))
  println(t)
  val t1 = treeFunctor.map(t)((x: Int) => x + x)
  println(f"After map (x + x) ${t1}")
  println(f"After map (x * x) with syntax ${t.map((x: Int) => x * x)}")
}
