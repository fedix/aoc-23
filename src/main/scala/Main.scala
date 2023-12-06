import days.*

import scala.io.Source
import scala.util.Using
import scala.util.chaining.*

def readInput(path: String): List[String] =
  Using(Source.fromResource(path))(_.getLines().toList).get

@main def hello: Unit =
  readInput("2.txt")
    .pipe(Day2.solve2)
    .pipe(println)

  // readInput("1-1.txt")
  //   .pipe(Day1.solve2)
  //   .pipe(println)
