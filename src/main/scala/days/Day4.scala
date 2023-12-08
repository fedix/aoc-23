package days
package Day4

case class Card(n: Int, winning: Set[Int], owned: Set[Int])

def parseInts(raw: String): Set[Int] =
  raw.trim.split(' ').filter(_.nonEmpty).map(_.toInt).toSet

def parseCard(raw: String): Option[Card] = raw match
  case s"Card $n: ${winningStr} | ${ownedStr}" =>
    Some(Card(n.trim.toInt, parseInts(winningStr), parseInts(ownedStr)))
  case _ => None

def solve1(input: List[String]): Int =
  input
    .flatMap(parseCard)
    .map(card =>
      val ownedWinning = card.winning.intersect(card.owned)
      math.pow(2, ownedWinning.size - 1).toInt
    )
    .sum

def solve2(input: List[String]): Int = {
  input
    .flatMap(parseCard)
    .foldLeft(Map.empty[Int, Int]) { (extraCardsAcc, card) =>
      val winningCount = card.winning.intersect(card.owned).size
      val wonCards = card.n + 1 to card.n + winningCount
      val inc = extraCardsAcc.getOrElse(card.n, 0) + 1

      wonCards.foldLeft(extraCardsAcc)((acc, wonCard) =>
        acc.updatedWith(wonCard) {
          _.map(_ + inc).orElse(Some(inc))
        }
      )
    }
    .values
    .sum + input.length
}
