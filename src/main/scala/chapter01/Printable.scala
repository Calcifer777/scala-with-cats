package chapter01

trait Printable[A] {
  def format(value: A): String
}

object Printable {

  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      def format(s: String): String = s
    }

  implicit val intPrintable: Printable[Int] =
    new Printable[Int] {
      def format(i: Int): String = i.toString()
    }

  implicit val catPrintable: Printable[Cat] =
    new Printable[Cat] {
      def format(c: Cat): String = f"${c.name} is a ${c.age} year-old ${c.color} cat."
    }

  def format[A](a: A)(implicit p: Printable[A]): String = p.format(a)

  def print[A](a: A)(implicit p: Printable[A]): Unit = println(format(a))
}
