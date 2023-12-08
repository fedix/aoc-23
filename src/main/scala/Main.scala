import days.*

import scala.io.Source
import scala.util.Using

def readInput(path: String): List[String] =
  Using(Source.fromResource(path))(_.getLines().toList).get

@main def hello: Unit =
  val input = readInput("4.txt")
  List(Day4.solve1(input), Day4.solve2(input)).foreach(println)
