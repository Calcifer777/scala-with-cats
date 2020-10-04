package chapter02

import org.scalatest._
import flatspec._
import matchers._
import should.Matchers._

import chapter02.Monoid.{ associativeLaw, identityLaw }

// BDD Testing
class MonoidsSpec extends AnyFlatSpec {
  import chapter02.Monoid.intMonoid
  "A Int monoid" should "respect the commutative law" in {
    assert(associativeLaw(3, 2, 2))
  }
}

// Property testing with scalacheck
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

class IntMonoidSpecification extends Properties("Int") {
  import chapter02.Monoid.intMonoid
  property("associativeLaw law") = forAll { 
    (x: Int, y: Int, z: Int) =>
      (associativeLaw(x, y, z)(intMonoid)).equals(true)
  }
  property("identityLaw law") = forAll { 
    (x: Int) => (identityLaw(x)).equals(true)
  }
}

class BooleanAndMonoidSpec extends Properties("BooleanAnd") {
  import chapter02.Monoid.booleanAndMonoid
  property("associativeLaw law") = forAll { 
    (x: Boolean, y: Boolean, z: Boolean) =>
      (associativeLaw(x, y, z)(booleanAndMonoid)).equals(true)
  }
  property("identityLaw law") = forAll { 
    (x: Boolean) => (identityLaw(x)(booleanAndMonoid)).equals(true)
  }
}

class BooleanOrMonoidSpec extends Properties("BooleanOr") {
  import chapter02.Monoid.booleanOrMonoid
  property("associativeLaw law") = forAll { 
    (x: Boolean, y: Boolean, z: Boolean) =>
      (associativeLaw(x, y, z)(booleanOrMonoid)).equals(true)
  }
  property("identityLaw law") = forAll { 
    (x: Boolean) => (identityLaw(x)(booleanOrMonoid)).equals(true)
  }
}

class SetUnionMonoidSpec extends Properties("SetUnion") {
  import chapter02.Monoid.setUnionMonoid
  property("associativeLaw law") = forAll { 
    (x: Set[Int], y: Set[Int], z: Set[Int]) =>
      (associativeLaw(x, y, z)(setUnionMonoid)).equals(true)
  }
  property("identityLaw law") = forAll { 
    (x: Set[Int]) => (identityLaw(x)(setUnionMonoid)).equals(true)
  }
}
