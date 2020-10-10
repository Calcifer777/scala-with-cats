package chapter03

trait Codec[A] {
  self =>
  def encode(value: A): String
  def decode(s: String): A
  def imap[B](dec: A => B)(enc: B => A): Codec[B] =
    new Codec[B] {
      def encode(value: B): String =
        (enc andThen self.encode)(value)
      def decode(s: String): B = dec(self.decode(s))
    }
}

object Codec {

  def encode[A](value: A)(implicit c: Codec[A]) =
    c.encode(value)

  def decode[A](s: String)(implicit c: Codec[A]) =
    c.decode(s)

  implicit val stringCodec: Codec[String] =
    new Codec[String] {
      def encode(value: String): String = value
      def decode(s: String): String     = s
    }

  implicit val intCodec: Codec[Int] =
    stringCodec.imap(_.toInt)(_.toString)

  implicit val doubleCodec: Codec[Double] =
    stringCodec.imap(_.toDouble)(_.toString)

}
