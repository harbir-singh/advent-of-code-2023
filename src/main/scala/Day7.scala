import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

object Day7 extends App {

  val source = Source.fromFile("src/main/resources/Day7.input")
  val input: List[String] = source.getLines.toList
  source.close()

  trait Hand {
    val hand: String
    val bid: Int
    val handRank: Int
  }

  case class FiveOfAKind(hand: String, bid: Int, handRank: Int = 7) extends Hand
  case class FourOfAKind(hand: String, bid: Int, handRank: Int = 6) extends Hand
  case class FullHouse(hand: String, bid: Int, handRank: Int = 5) extends Hand
  case class ThreeOfAKind(hand: String, bid: Int, handRank: Int = 4) extends Hand
  case class TwoPair(hand: String, bid: Int, handRank: Int = 3) extends Hand
  case class OnePair(hand: String, bid: Int, handRank: Int = 2) extends Hand
  case class HighCard(hand: String, bid: Int, handRank: Int = 1) extends Hand

  def parseRow(row: String, joker: Option[Char] = None): Hand = {
    def classifyHand(hand: String, bid: String, joker: Option[Char] = None): Hand = {
      val handMap: Map[Char, Int] =
        hand.split("").map(_.head).groupBy(identity).view.mapValues(_.length).toMap
      val sorted = handMap.values.toList.sorted
      val handRank = joker.fold(sorted) { j =>
        if (handMap.contains(j) && handMap.size > 1) {
          val old = handMap.removed(j).values.toList.sorted
          old.init :+ old.last + handMap(j)
        } else sorted
      }
      handRank match {
        case List(1, 1, 1, 1, 1) => HighCard(hand, bid.toInt)
        case List(1, 1, 1, 2)    => OnePair(hand, bid.toInt)
        case List(1, 2, 2)       => TwoPair(hand, bid.toInt)
        case List(1, 1, 3)       => ThreeOfAKind(hand, bid.toInt)
        case List(2, 3)          => FullHouse(hand, bid.toInt)
        case List(1, 4)          => FourOfAKind(hand, bid.toInt)
        case List(5)             => FiveOfAKind(hand, bid.toInt)
      }
    }

    row match {
      case s"$hand $bid" => classifyHand(hand, bid, joker)
    }
  }

  def sortHand(a: Hand, b: Hand, joker: Option[Char] = None): Boolean = {
    def compareCards(a: Char, b: Char, joker: Option[Char] = None): Boolean = {
      val cards: List[Char] =
        (2 to 9).toList.map(n => (n + '0').toChar) ++ List('T', 'J', 'Q', 'K', 'A')

      val cardsWithJoker: List[Char] = joker.fold(cards) { j =>
        j +: cards.filterNot(_ == j)
      }

      val rankByCard: Map[Char, Int] = cardsWithJoker.zipWithIndex.toMap
      rankByCard(a) < rankByCard(b)
    }

    @tailrec
    def aux(index: Int = 0): Boolean =
      if (index == a.hand.length)
        false
      else if (a.handRank != b.handRank)
        a.handRank < b.handRank
      else if (a.hand(index) != b.hand(index))
        compareCards(a.hand(index), b.hand(index), joker)
      else
        aux(index + 1)

    aux()
  }

  def part1(joker: Option[Char] = None): Int =
    input
      .map(r => parseRow(r, joker))
      .groupBy(_.handRank)
      .toSeq
      .sortBy(_._1)
      .map(_._2)
      .flatMap(x => x.sortWith((a, b) => sortHand(a, b, joker)))
      .zipWithIndex
      .map { case (hand, index) => (index + 1) * hand.bid }
      .sum

  def part2: Int = part1(Option('J'))

  println(s"part1=${part1()}") // 249483956
  println(s"part2=$part2") // 252137472
}
