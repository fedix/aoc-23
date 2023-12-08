package days
package Day3

import Cell.*
import scala.annotation.tailrec

enum Cell:
  case Number(n: Int)
  case Symbol(ch: Char)
  case Dot

type Schematic = Vector[Vector[Cell]]

type Pos = (Int, Int)

@tailrec
def parseRow(str: String, acc: Vector[Cell] = Vector.empty): Vector[Cell] =
  str.headOption match
    case Some(ch) =>
      if (ch.isDigit) {
        val (nums, rest) = str.span(_.isDigit)
        parseRow(
          rest,
          acc ++ nums.map(_ => Number(nums.toInt))
        )
      } else if (ch == '.') parseRow(str.tail, acc :+ Dot)
      else parseRow(str.tail, acc :+ Symbol(ch))
    case None => acc

def parseSchematic(raw: List[String]): Schematic =
  raw.foldLeft(Vector.empty) {
    _ :+ parseRow(_)
  }

def getNeighbors(schematic: Schematic, pos: Pos): List[Cell] = {
  val offsets = List(-1, 0, 1)

  for {
    di <- offsets
    dj <- offsets
    newPos = (pos._1 + di) -> (pos._2 + dj) if newPos != pos
    cell <- schematic.lift(newPos._1).flatMap(_.lift(newPos._2))
  } yield cell
}

def solve1(input: List[String]): Int =
  val schematic = parseSchematic(input)

  schematic.zipWithIndex
    .foldLeft(List.empty[Int]) { case (acc, (row, i)) =>
      val rowNumbers = row.zipWithIndex.foldLeft(List.empty[Int]) {
        case (rowAcc, (cell, j)) =>
          val neighborNumbers = cell match
            case Symbol(_) =>
              getNeighbors(schematic, (i, j)).collect { case Number(n) =>
                n
              }.distinct
            case _ => Nil

          rowAcc ::: neighborNumbers
      }
      acc ::: rowNumbers
    }
    .sum

def solve2(input: List[String]): Int = {
  val schematic = parseSchematic(input)

  schematic.zipWithIndex
    .foldLeft(List.empty[Int]) { case (acc, (row, i)) =>
      val rowNumbers = row.zipWithIndex.foldLeft(List.empty[Int]) {
        case (rowAcc, (cell, j)) =>
          val neighborNumbers = cell match
            case Symbol('*') =>
              getNeighbors(schematic, (i, j)).collect { case Number(n) =>
                n
              }.distinct
            case _ => Nil

          neighborNumbers match
            case fst :: snd :: Nil => fst * snd :: rowAcc
            case _                 => rowAcc
      }
      acc ::: rowNumbers
    }
    .sum
}
