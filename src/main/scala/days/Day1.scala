package days

import scala.util.chaining.*

import scala.compiletime.ops.string

object Day1 {
  val digitMapping = Map(
    "one" -> '1',
    "two" -> '2',
    "three" -> '3',
    "four" -> '4',
    "five" -> '5',
    "six" -> '6',
    "seven" -> '7',
    "eight" -> '8',
    "nine" -> '9'
  )

  def safeIndexOf(source: String)(subStr: String): Option[Int] = {
    val pos = source.indexOf(subStr)
    if (pos != -1) Some(pos)
    else None
  }

  def indexesOf(source: String)(subStr: String): Set[Int] =
    source.tails.zipWithIndex.flatMap { (sub, i) =>
      safeIndexOf(sub)(subStr).map(_ + i)
    }.toSet

  def solve1(lines: List[String]) =
    lines
      .flatMap(s =>
        for {
          first <- s.find(_.isDigit)
          last <- s.findLast(_.isDigit)
          number <- s"$first$last".toIntOption
        } yield number
      )
      .reduce(_ + _)

  def solve2(lines: List[String]) =
    lines
      .flatMap { line =>
        val spelledDigits = digitMapping.keySet.filter(line.contains)
        val digits =
          List(line.find(_.isDigit), line.findLast(_.isDigit)).flatten

        val allDigits =
          digits.flatMap(ch => indexesOf(line)(ch.toString).map(_ -> ch)) ++
            spelledDigits
              .flatMap(d => indexesOf(line)(d).map(_ -> digitMapping(d)))

        val (_, first) = allDigits.minBy(_._1)
        val (_, last) = allDigits.maxBy(_._1)

        s"$first$last".toIntOption
      }
      .reduce(_ + _)
}
