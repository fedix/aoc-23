package days
package Day6

def parseNums(raw: String): List[Long] =
  raw.split(" ").flatMap(_.toLongOption).toList

def parseRaces(input: List[String]): List[(Long, Long)] =
  parseNums(input.head).zip(parseNums(input.last))

def parseRace(input: List[String]): (Long, Long) =
  (
    parseNums(input.head).mkString.toLong,
    parseNums(input.last).mkString.toLong
  )

def simulate(time: Long): List[Long] =
  (0L to time)
    .map(hold => (time - hold) * hold)
    .toList

def solve1(input: List[String]): Long = {
  val races = parseRaces(input)
  races
    .map((time, distance) => simulate(time).count(_ > distance))
    .product
}

def solve2(input: List[String]): Long = {
  val (time, distance) = parseRace(input)
  simulate(time).count(_ > distance)
}
