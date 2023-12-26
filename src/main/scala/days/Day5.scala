package days
package Day5

import scala.annotation.tailrec

case class Range(start: Long, length: Long) {
  def end: Long = start + length

  def contains(n: Long): Boolean =
    n >= start && n <= end

  def overlaps(other: Range): Boolean =
    contains(other.start) || contains(other.end) ||
      other.contains(start) || other.contains(end)

  // unsafe: should be used only for overlapping ranges
  def splitWith(other: Range): List[Range] =
    List(
      Option.when(start < other.start)(Range(start, other.start - start)),
      Option.when(end > other.end)(Range(other.end, end - other.end))
    ).flatten
}

case class Segment(sourceStart: Long, destStart: Long, length: Long) {
  def sourceRange: Range = Range(sourceStart, length)
}

object Segment {
  def applyMapping(src: Long): PartialFunction[Segment, Long] = {
    case seg if seg.sourceRange.contains(src) =>
      val offset = src - seg.sourceStart
      seg.destStart + offset
  }
}

enum Category:
  case seed, soil, fertilizer, water, light, temperature, humidity, location

case class Mapping(source: Category, dest: Category, segments: List[Segment])

type Almanac = List[Mapping]

type Path = List[(Category, Long)]
type RangePath = List[(Category, List[Range])]

def parseSeeds(raw: String): List[Long] =
  raw.split(' ').flatMap(_.toLongOption).toList

def parseSeedRanges(raw: String): Iterator[Range] =
  parseSeeds(raw)
    .grouped(2)
    .map {
      case start :: length :: Nil => Range(start, length)
      case other => throw new Exception(s"Unexpected input ${other}")
    }

def parseMapping(raw: List[String]): Mapping = {
  val (source, dest) = raw.head match {
    case s"${source}-to-${dest} map:" => source -> dest
  }

  val mapping = raw.tail
    .map(_.split(' ').map(_.toLong))
    .map { case Array(destStart, sourceStart, rangeLen) =>
      Segment(sourceStart, destStart, rangeLen)
    }

  Mapping(Category.valueOf(source), Category.valueOf(dest), mapping)
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

def mapRange(sourceRange: Range, mapping: Mapping): List[Range] = {
  val (mapped, unmapped) =
    mapping.segments.foldLeft((List.empty[Range], List(sourceRange))) {
      case ((accRanges, left), seg) =>
        left match {
          case Nil => accRanges -> left
          case curRanges =>
            val res = curRanges.map(applyMapping(_, seg))
            (accRanges ::: res.flatMap(_._1), res.flatMap(_._2))
        }
    }
  mapped ++ unmapped
}

def applyMapping(
    sourceRange: Range,
    seg: Segment
): (Option[Range], List[Range]) = {
  val mappingRange = seg.sourceRange
  if (sourceRange.overlaps(mappingRange)) {
    val start = sourceRange.start max mappingRange.start
    val end = sourceRange.end min mappingRange.end
    val len = end - start

    val offset = start - mappingRange.start
    val range = Range(seg.destStart + offset, len)

    val left = sourceRange.splitWith(Range(start, len))

    Some(range) -> left
  } else None -> List(sourceRange)
}

def findLocation(sourceCat: Category, source: Long, almanac: Almanac): Path = {
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

def findLocationRange(
    sourceCat: Category,
    source: Range,
    almanac: Almanac
): RangePath = {

  @tailrec
  def go(sourceCat: Category, source: List[Range], acc: RangePath): RangePath =
    almanac.find(_.source == sourceCat) match
      case None => acc
      case Some(mapping) =>
        val mapped = source.flatMap(mapRange(_, mapping))

        go(
          mapping.dest,
          mapped,
          (mapping.dest -> mapped) :: acc
        )

  go(sourceCat, List(source), List(sourceCat -> List(source)))
}

def solve1(input: List[String]): Long = {
  val seeds = parseSeeds(input.head)
  val almanac = parseAlmanac(input.tail, List.empty)
  val paths = seeds.map(seed => findLocation(Category.seed, seed, almanac))

  paths
    .map(_.head._2)
    .min
}

def solve2(input: List[String]): Long = {
  val seedRanges = parseSeedRanges(input.head)
  val almanac = parseAlmanac(input.tail, List.empty)
  val paths =
    seedRanges
      .map(range => range -> findLocationRange(Category.seed, range, almanac))

  val minLocationPath =
    paths
      .minBy((_, path) => path.head._2.map(_.start).min)
      ._2

  val minLocation = minLocationPath.head._2.map(_.start).min
  minLocation

  // 15290096
}
