import days.*

import scala.io.Source
import scala.util.Using

def readInput(path: String): List[String] =
  Using(Source.fromResource(path))(_.getLines().toList).get

@main def main: Unit =
  val input = readInput("6.txt")
  List(Day6.solve1(input), Day6.solve2(input)).foreach(println)
