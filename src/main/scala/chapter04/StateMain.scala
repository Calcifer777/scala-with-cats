package chapter04

import scala.language.higherKinds

import cats.data.State
import cats.syntax.applicative._
import State._


object PostOrderCalculator extends App {

  type CalcState[A] = State[List[Int], A]

  def operand(num: Int): CalcState[Int] = 
    State[List[Int], Int] { stack => 
      (num :: stack, num)
    }

  def operator(func: (Int, Int) => Int) : CalcState[Int] = 
    State[List[Int], Int] {
      l => l match {
        case b :: a :: tail =>
          val ans = func(a, b)
          (ans :: tail, ans)
        case _ => sys.error("Fail")
      }
    }

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator((a: Int, b: Int) => a - b)
      case "*" => operator((a: Int, b: Int) => a * b)
      case "/" => operator((a: Int, b: Int) => a / b)
      case num => operand(num.toInt)
    }

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) {
      (acc, x) => acc.flatMap(_ => evalOne(x))
    }

  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    ans <- evalOne("+")
  } yield ans

  def evalInput(s: String): Int = {
    val inputs: List[String] = s.toList.map(_.toString)
    val program = evalAll(inputs)
    val (state, result) = program.run(Nil).value
    result
  }

  val testProgram = "32+1-4*2/"
  println(s"The result of ${testProgram} is: ${evalInput(testProgram)})")

}
