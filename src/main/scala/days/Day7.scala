package days
package Day7

import cats.syntax.traverse.*
import HandType.*
import util.|>

enum HandType:
  case HighCard, OnePair, TwoPair, Three, FullHouse, Four, Five

type Hand = List[Int]
type Result[A] = Either[String, A]

case class TypedHand(cards: Hand, tp: HandType)

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

def parseLine(line: String): Result[(Hand, Int)] =
  line match
    case s"$handRaw $bidRaw" =>
      for
        hand <- parseHand(handRaw)
        bid <- bidRaw.toIntOption.toRight("Non integer bid")
      yield hand -> bid
    case _ => Left("Unexpected line format")

def determineType(hand: Hand): TypedHand = {
  val counts = hand.groupBy(identity).values.map(_.length)
  val tp = counts.max match
    case 5                                    => Five
    case 4                                    => Four
    case 3 if counts.find(_ == 2).isDefined   => FullHouse
    case 3                                    => Three
    case 2 if counts.filter(_ == 2).size == 2 => TwoPair
    case 2                                    => OnePair
    case _                                    => HighCard

  TypedHand(hand, tp)
}

def promote: TypedHand => TypedHand =
  case hand if hand.cards.contains(1) =>
    hand.copy(tp = hand.tp match
      case HighCard => OnePair
      case OnePair  => Three
      case TwoPair =>
        if (hand.cards.count(_ == 1) == 1) FullHouse
        else Four
      case Three                   => Four
      case FullHouse | Four | Five => Five
    )
  case hand => hand

implicit val handOrdering: Ordering[Hand] = (x: Hand, y: Hand) =>
  x.zip(y)
    .collectFirst { case (a, b) if a != b => a.compare(b) }
    .getOrElse(0)

implicit val typedHandOrdering: Ordering[TypedHand] =
  Ordering.by[TypedHand, (Int, Hand)] { hand =>
    hand.tp.ordinal -> hand.cards
  }

def mapJokers: Hand => Hand =
  _.map {
    case 12    => 1
    case other => other
  }

def solve1(input: List[String]): Result[Long] =
  input
    .traverse(parseLine)
    .map(lines =>
      lines
        .sortBy((hand, _) => determineType(hand))
        .zipWithIndex
        .map { case ((_, bid), rank) => bid * (rank + 1) }
        .sum
    )
  // 253638586

def solve2(input: List[String]): Result[Long] =
  input
    .traverse(parseLine)
    .map(lines =>
      lines
        .sortBy((hand, _) => mapJokers(hand) |> determineType |> promote)
        .zipWithIndex
        .map { case ((_, bid), rank) => bid * (rank + 1) }
        .sum
    )
  // 253253225
