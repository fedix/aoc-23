package days
package Day2

import math.max

case class CubeSet(red: Int, green: Int, blue: Int) {
  def power: Int = red * green * blue
}

object CubeSet {
  def zeros = CubeSet(0, 0, 0)
}

case class Game(
    id: Int,
    sets: List[CubeSet]
)

def parseGame(raw: String): Option[Game] =
  raw match
    case s"Game $id: ${sets}" =>
      Some(Game(id.toInt, sets.split(';').map(parseSet).toList))
    case _ => None

def parseSet(raw: String): CubeSet =
  raw.split(',').foldLeft(CubeSet.zeros) { (acc, str) =>
    str.trim match
      case s"$n green" => acc.copy(green = n.toInt)
      case s"$n red"   => acc.copy(red = n.toInt)
      case s"$n blue"  => acc.copy(blue = n.toInt)
  }

val constraint = CubeSet(red = 12, green = 13, blue = 14)

def solve1(input: List[String]): Int =
  input
    .flatMap(parseGame)
    .filter { game =>
      game.sets.forall(set =>
        set.red <= constraint.red && set.blue <= constraint.blue && set.green <= constraint.green
      )
    }
    .map(_.id)
    .sum

def solve2(input: List[String]): Int =
  input
    .flatMap(parseGame)
    .map { game =>
      game.sets.reduce { (cs1, cs2) =>
        CubeSet(
          max(cs1.red, cs2.red),
          max(cs1.green, cs2.green),
          max(cs1.blue, cs2.blue)
        )
      }
    }
    .map(_.power)
    .sum
