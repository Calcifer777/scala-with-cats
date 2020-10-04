package chapter02

import Monoid._

final case class Order(totalCost: Double, quantity: Double)

object Order {
  implicit val orderMonoid: Monoid[Order] =
    new Monoid[Order] {
      def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
      def empty: Order = Order(0.0, 0.0)
    }
}
