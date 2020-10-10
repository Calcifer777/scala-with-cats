package chapter03

final case class Box[A](value: A)

object Box {

  implicit def boxPrintable[A](
      implicit printable: Printable[A]
  ): Printable[Box[A]] =
    printable.contramap[Box[A]](_.value)

  implicit def boxCodec[A](
      implicit codec: Codec[A]
  ): Codec[Box[A]] =
    codec.imap[Box[A]](Box(_))(_.value)
}
