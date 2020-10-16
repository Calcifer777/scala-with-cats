package chapter10

import User._


object Main extends App {
  
  val r1 = createUser("Noel", "noel@underscore.io")
  println(r1)

  val r2 = createUser("", "dave@underscore.io@io")
  println(r2)

}
