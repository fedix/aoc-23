package days
package Day7

import cats.syntax.traverse.*
import HandType.*
import util.*

type Hand = List[Int]

enum HandType:
  case HighCard, OnePair, TwoPair, Three, FullHouse, Four, Five

case class TypedHand(cards: Hand, tp: HandType)

case class Play(hand: Hand, bid: Int)

given Ordering[Hand] = (x: Hand, y: Hand) =>
  x.zip(y)
    .collectFirst { case (a, b) if a != b => a.compare(b) }
    .getOrElse(0)

given Ordering[TypedHand] =
  Ordering.by { hand => hand.tp.ordinal -> hand.cards }

def parseHand(raw: String): Result[Hand] =
  raw.toList.traverse { ch =>
    charToStrength.lift(ch).toRight(s"Unexpected char $ch")
  }

def charToStrength: PartialFunction[Char, Int] =
  case n if n.asDigit >= 2 && n.asDigit <= 9 => n.asDigit
  case 'T'                                   => 11
  case 'J'                                   => 12
  case 'Q'                                   => 13
  case 'K'                                   => 14
  case 'A'                                   => 15

def parseLine(line: String): Result[Play] =
  line match
    case s"$handRaw $bidRaw" =>
      for
        hand <- parseHand(handRaw)
        bid <- bidRaw.toIntOption.toRight("Non integer bid")
      yield Play(hand, bid)
    case _ => Left("Unexpected line format")

def determineType(hand: Hand): TypedHand = {
  val jokers = hand.count(_ == 1)

  val counts = hand
    .filter(_ != 1)
    .groupBy(identity)
    .values
    .toList
    .map(_.length)
    .sorted
    .reverse

  val tp = counts.headOption
    .map(h => counts.updated(0, h + jokers))
    .getOrElse(5 :: Nil) match
    case 5 :: _           => Five
    case 4 :: _           => Four
    case 3 :: 2 :: _      => FullHouse
    case 3 :: 1 :: 1 :: _ => Three
    case 2 :: 2 :: _      => TwoPair
    case 2 :: _           => OnePair
    case _                => HighCard

  TypedHand(hand, tp)
}

def downgrade(p: Play): Play =
  p.copy(hand = p.hand.map {
    case 12 => 1
    case n  => n
  })

def winnings(plays: List[Play]): Int =
  plays
    .sortBy(_.hand |> determineType)
    .zipWithIndex
    .map { case (play, rank) => play.bid * (rank + 1) }
    .sum

def solve1(input: List[String]): Result[Long] =
  input
    .traverse(parseLine)
    .map(winnings)
  // 253638586

def solve2(input: List[String]): Result[Long] =
  input
    .traverse(parseLine)
    .map(lines => lines.map(downgrade) |> winnings)
  // 253253225
