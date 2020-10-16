package chapter11

import cats.syntax.semigroup._
import cats.kernel.CommutativeMonoid
import BoundedSemiLattice._

trait GCounter[F[_,_],K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]
  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]
  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}

object GCounter {

  import KeyValueStore._

  def apply[F[_,_], K, V](implicit counter: GCounter[F, K, V]) = counter

  implicit def mapGCounterInstance[K, V]: GCounter[Map, K, V] =
    new GCounter[Map, K, V] {
      def increment(map: Map[K, V])(k: K, v: V)(implicit cm: CommutativeMonoid[V]): Map[K,V] = {
        val value = v |+| map.getOrElse(k, cm.empty)
        map + (k -> value)
      }
      def merge(map1: Map[K,V], map2: Map[K,V])(implicit b: BoundedSemiLattice[V]): Map[K,V] = map1 |+| map2
      def total(f: Map[K,V])(implicit m: CommutativeMonoid[V]): V = f.values.fold(m.empty)(m.combine)
    }

  implicit def gcounterInstance[F[_,_], K, V](implicit kvs: KeyValueStore[F], km: CommutativeMonoid[F[K, V]]) =
    new GCounter[F, K, V] {
      def increment(f: F[K, V])(key: K, value: V)(implicit m: CommutativeMonoid[V]): F[K, V] = {
        val total = f.getOrElse(key, m.empty) |+| value
        f.put(key, total)
      }
      def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] = f1 |+| f2
      def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V = m.combineAll(f.values)
    }

}

