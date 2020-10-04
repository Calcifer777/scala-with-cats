trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {

  def apply[A](implicit monoid: Monoid[A]) =
    monoid

  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]) =
    m.combine(m.combine(x, y), z) == m.combine(x, m.combine(y, z))

  def identityLaw[A](x: A)(implicit m: Monoid[A]) =
    m.combine(x, m.empty) == x && m.combine(m.empty, x) == x

  implicit val intMonoid: Monoid[Int] =
    new Monoid[Int] {
      def combine(x: Int, y: Int): Int = x + y
      def empty: Int                   = 0
    }

  implicit val stringMonoid: Monoid[String] =
    new Monoid[String] {
      def combine(x: String, y: String): String = x + y
      def empty: String                         = ""
    }

  implicit val booleanAndMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(x: Boolean, y: Boolean): Boolean = x && y
      def empty: Boolean                           = true
    }

  implicit val booleanOrMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(x: Boolean, y: Boolean): Boolean = x || y
      def empty: Boolean                           = false
    }

  implicit def setUnionMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def combine(x: Set[A], y: Set[A]): Set[A] = x union y
      def empty: Set[A]                           = Set[A]()
    }

}
