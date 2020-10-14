package chapter09

import cats.{ Monoid, Foldable, Traverse }
import cats.syntax.monoid._
import cats.syntax.applicative._
import cats.syntax.foldable._
import cats.syntax.traverse._
import cats.instances.int._
import cats.instances.string._
import cats.instances.vector._
import cats.instances.future._

import scala.concurrent.{ Future, Await }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

trait MapReducer {

  def foldMap[A, B: Monoid](v: Vector[A])(f: A => B): B = 
    v.map(f).fold(Monoid[B].empty)(Monoid[B].combine)

  def parallelFoldMap[A, B: Monoid](v: Vector[A])(f: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors()
    val groupSize = (1.0 * v.size / numCores).ceil.toInt
    val batches: Iterator[Vector[A]] = v.grouped(groupSize)
    val partialsList: Iterator[Future[B]] = batches
      .map { (b: Vector[A]) => Future(foldMap(b)(f)) } 
    for {
      partials <- Future.sequence(partialsList)
    } yield partials.fold(Monoid[B].empty)(Monoid[B].combine)
  }

  def parallelFoldMapWithCats[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors()
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    values
      .grouped(groupSize)
      .toVector
      .traverse(batch => Future(batch.toVector.foldMap(f)))
      .map(_.combineAll)
  }

}

object Main extends App with MapReducer {
  val r1 = foldMap(Vector(1, 2, 3))(identity)
  println(r1)

  val r2 = foldMap(Vector(1, 2, 3))(_.toString + "!")
  println(r2)

  val result: Future[Int] = parallelFoldMap((1 to 1000000).toVector)(identity)
  val rPar1 = Await.result(result, 1.second)
  println(rPar1)

}

