package chapter03

trait Printable[A] {

  self => 
  
  def format(value: A): String 

  def contramap[B](f: B => A): Printable[B] = 
    new Printable[B] {
      def format(value: B): String = 
        self.format(f(value))
    }
}

object Printable {
  implicit val stringPrintable: Printable[String] = 
    new Printable[String] {
      def format(value: String): String = f"/* ${value} */"
    }
  implicit val booleanPrintable: Printable[Boolean] = 
    new Printable[Boolean] {
      def format(value: Boolean): String = 
        if (value) "yes" else "no"
    }

  def format[A](value: A)(implicit p: Printable[A]) = 
    p.format(value)
    
}
