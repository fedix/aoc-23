package days
package Day2

import math.max

enum Color:
  case R, G, B

case class Constraint(red: Int, green: Int, blue: Int)

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

def parseGame(raw: String): Game =
  raw match
    case s"Game $id: ${sets}" =>
      Game(id.toInt, sets.split(';').map(parseSet).toList)

def parseSet(raw: String): CubeSet =
  raw.split(',').foldLeft(CubeSet.zeros) { (acc, str) =>
    str.trim match
      case s"$n green" => acc.copy(green = n.toInt)
      case s"$n red"   => acc.copy(red = n.toInt)
      case s"$n blue"  => acc.copy(blue = n.toInt)
  }

val constraint = Constraint(red = 12, green = 13, blue = 14)

def solve1(input: List[String]): Int =
  input
    .map(parseGame)
    .filter { game =>
      game.sets.forall(set =>
        set.red <= constraint.red && set.blue <= constraint.blue && set.green <= constraint.green
      )
    }
    .foldLeft(0)((acc, g) => acc + g.id)

def solve2(input: List[String]): Int =
  input
    .map(parseGame)
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
    .reduce(_ + _)
