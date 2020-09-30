package chapter01

case class Person(name: String, email: String)

object Person {
  implicit class JsonWriterOps[Person](value: Person) {
    def toJson(implicit w: JsonWriter[Person]): Json = w.write(value)
  }
}
