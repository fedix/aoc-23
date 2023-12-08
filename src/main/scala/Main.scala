import days.*

import scala.io.Source
import scala.util.Using
import scala.util.chaining.*

def readInput(path: String): List[String] =
  Using(Source.fromResource(path))(_.getLines().toList).get

@main def hello: Unit =
  val input = readInput("3.txt")
  List(Day3.solve1(input), Day3.solve2(input)).foreach(println)
