import days.*

import util.inputLines

@main def main: Unit =
  val input = inputLines("7.txt")
  List(Day7.solve1(input), Day7.solve2(input)).foreach(println)
