package chapter02

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Arbitrary}

import chapter02.Monoid._
import chapter02.Order.orderMonoid

object Utils {
  val orderGen: Gen[Order] = for {
    totalCost <- Gen.choose(0, 1000)
    quantity <- Gen.choose(0, 1000)
  } yield Order(totalCost, quantity)
  implicit val arbOrder: Arbitrary[Order] = Arbitrary(orderGen)
}

class OrderMonoidGenSpec extends Properties("OrderMonoid with Generators") {

  import Utils.orderGen

  property("identityLaw law") = forAll(orderGen) { 
    (o: Order) => (identityLaw(o)(orderMonoid)).equals(true)
  }
  
  property("associativeLaw law") = forAll(orderGen, orderGen, orderGen) { 
    (x: Order, y: Order, z: Order) => 
      (associativeLaw(x, y, z)(orderMonoid)).equals(true)
  }

}

class OrderMonoidArbSpec extends Properties("OrderMonoid with Arbitraries") {

  import Utils.arbOrder

  property("identityLaw law") = forAll { 
    (o: Order) => (identityLaw(o)(orderMonoid)).equals(true)
  }

  property("associativeLaw law") = forAll { 
    (x: Order, y: Order, z: Order) => 
      (associativeLaw(x, y, z)(orderMonoid)).equals(true)
  }

}
