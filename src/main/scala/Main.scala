import days.*

import scala.io.Source
import scala.util.Using

def readInput(path: String): List[String] =
  Using(Source.fromResource(path))(_.getLines().toList).get

@main def main: Unit =
  val input = readInput("7.txt")
  List(Day7.solve1(input), Day7.solve2(input)).foreach(println)
