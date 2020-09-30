package chapter01

object JsonMain extends App {

  val p = Person("Dave", "dave@example.com")

  val jsonPerson = Json.toJson(p)

  val jsonPersonWithSyntax = p.toJson
  // equivalent to: Person("Dave", "dave@example.com").toJson(personWriter)

  println(jsonPerson)

  // implicitly yields the implicit value
  def personToJson(p: Person): Json = 
    implicitly[JsonWriter[Person]].write(p)

  println(personToJson(p))
  
}
