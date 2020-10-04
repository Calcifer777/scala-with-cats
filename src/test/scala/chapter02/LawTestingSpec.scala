// Law testing with cats
import cats._
import cats.implicits._

import org.scalacheck.{Arbitrary, Gen}

// import cats.syntax.monoid._
// import org.scalatest.funsuite.AnyFunSuite
// import org.typelevel.discipline.scalatest.Discipline
// import org.scalatest.prop.Configuration
import cats.kernel.laws.discipline.{MonoidTests, SemigroupTests}
import cats.laws.discipline.FunctorTests

import org.scalacheck.ScalacheckShapeless._  
// allows not to define Arbitrary and eq instances

import cats.tests.CatsSuite  // implements checkAll

sealed trait Tree[+A]
case object Leaf extends Tree[Nothing]
case class Node[A](p: A, left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  implicit def semigroupTree[A: Semigroup]: Semigroup[Tree[A]] = new Semigroup[Tree[A]] {
    def combine(x: Tree[A], y: Tree[A]) = (x, y) match {
      case (Leaf, _) => Leaf
      case (_, Leaf) => Leaf
      case (Node(xp, xLeft, xRight), Node(yp, yLeft, yRight)) =>
        Node(xp |+| yp, xLeft |+| yLeft, xRight |+| yRight)
    }
  }
  implicit val functorTree: Functor[Tree] = new Functor[Tree] {
    def map[A, B](tree: Tree[A])(f: A => B) = tree match {
      case Leaf => Leaf
      case Node(p, left, right) => Node(f(p), map(left)(f), map(right)(f))
    }
  }
  implicit def eqTree[A: Eq]: Eq[Tree[A]] = Eq.fromUniversalEquals
}

class TreeLawTests extends CatsSuite {
  checkAll("Tree[Int].SemigroupLaws", SemigroupTests[Tree[Int]].semigroup)
  checkAll("Tree[Int].FunctorLaws", FunctorTests[Tree].functor[Int, Int, String])
}


