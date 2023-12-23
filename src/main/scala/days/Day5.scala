package days
package Day5

import scala.annotation.tailrec

// case class Range(start: Long, length: Long) {
//   def end = start + length

//   def overlaps(other: Range) = {
//     (start >= other.start && start <= other.end) || (end >= other.start && end <= other.end)
//   }
// }

case class Segment(sourceStart: Long, destStart: Long, length: Long) {
  def contains(n: Long): Boolean =
    n >= sourceStart && (n <= sourceStart + length)

  def invert = Segment(destStart, sourceStart, length)
}

object Segment {
  def applyMapping(src: Long): PartialFunction[Segment, Long] = {
    case seg if seg.contains(src) =>
      val offset = src - seg.sourceStart
      seg.destStart + offset
  }
}

enum Category:
  case Seed, Soil, Fertilizer, Water, Light, Temperature, Humidity, Location

case class Mapping(source: Category, dest: Category, segments: List[Segment]) {
  def invert = Mapping(dest, source, segments.map(_.invert))
}

object Mapping {
  def combine(a: Mapping, b: Mapping): Mapping = ???
}

type Almanac = List[Mapping]

type Path = List[(Category, Long)]

def parseSeeds(raw: String): List[Long] =
  raw.split(' ').flatMap(_.toLongOption).toList

def parseSeedRanges(raw: String): List[Segment] =
  raw
    .split(' ')
    .flatMap(_.toLongOption)
    .grouped(2)
    .map { case Array(start, length) => Segment(start, start, length) }
    .toList

def parseMapping(raw: List[String]): Mapping = {
  val (source, dest) = raw.head match {
    case s"${source}-to-${dest} map:" => source -> dest
    case _                            => ???
  }

  val mapping = raw.tail
    .map(_.split(' ').map(_.toLong))
    .map {
      case Array(destStart, sourceStart, rangeLen) =>
        Segment(sourceStart, destStart, rangeLen)
      case _ => ???
    }

  Mapping(
    Category.valueOf(source.capitalize),
    Category.valueOf(dest.capitalize),
    mapping
  )
}

@tailrec
def parseAlmanac(input: List[String], acc: Almanac): Almanac = {
  val (rawMapping, rest) = input.drop(1).span(_.nonEmpty)

  rawMapping match
    case Nil => acc
    case _   => parseAlmanac(rest, parseMapping(rawMapping) :: acc)
}

def map1(source: Long, mapping: Mapping): Long =
  mapping.segments
    .collectFirst(Segment.applyMapping(source))
    .getOrElse(source)

def findLocation(sourceCat: Category, source: Long, almanac: Almanac): Path = {
  println(s"find loc for ${sourceCat -> source}")
  @tailrec
  def go(sourceCat: Category, source: Long, acc: Path): Path =
    almanac.find(_.source == sourceCat) match
      case None => acc
      case Some(mapping) =>
        val mapped = map1(source, mapping)
        go(
          mapping.dest,
          mapped,
          (mapping.dest -> mapped) :: acc
        )

  go(sourceCat, source, List(sourceCat -> source))
}

def solve1(input: List[String]): Long = {
  val seeds = parseSeeds(input.head)
  val almanac = parseAlmanac(input.tail, List.empty)
  val paths = seeds.map(seed => findLocation(Category.Seed, seed, almanac))

  paths
    .map(_.map(_._2).mkString(" <- "))
    .foreach(println)

  paths
    .map(_.head._2)
    .min
}

def solve2(input: List[String]): Long = {
  0
}
