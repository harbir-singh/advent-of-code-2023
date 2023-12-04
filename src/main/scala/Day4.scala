import scala.annotation.tailrec
import scala.io.Source

object Day4 extends App {

  val source = Source.fromFile("src/main/resources/Day4.input")
  val input: List[String] = source.getLines.toList
  source.close()

  def parseRow(s: String): (Int, List[Int]) = s match {
    case s"Card ${cardNum}: $winningNumbers | $myNumbers" =>
      val wn = winningNumbers.split(" ").filterNot(_.isBlank).map(_.toInt)
      val mn = myNumbers.split(" ").filterNot(_.isBlank).map(_.toInt)
      (cardNum.trim.toInt, mn.intersect(wn).toList)
  }

  def calculatePoints(matches: List[Int]): Int =
    if (matches.isEmpty)
      0
    else
      matches.tail.foldLeft(1)((acc, _) => acc * 2)

  def part1: Int = input.map(r => parseRow(r)._2).map(calculatePoints).sum

  def part2: Int = {
    val cardCount: Map[Int, Int] = (1 to input.size).map(_ -> 1).toMap

    @tailrec
    def updateCardCount(cardId: Int, numMatches: Int, cardCount: Map[Int, Int]): Map[Int, Int] =
      if (numMatches > 0) {
        val incr = cardCount.getOrElse(cardId, 0)
        val existing = cardCount.getOrElse(cardId + numMatches, 0)
        updateCardCount(
          cardId,
          numMatches - 1,
          cardCount.updated(cardId + numMatches, existing + incr),
        )
      } else
        cardCount

    input
      .map { r =>
        val p = parseRow(r)
        p._1 -> p._2.size
      }
      .foldLeft(cardCount)((acc, t) => updateCardCount(t._1, t._2, acc))
      .values
      .sum

  }

  println(s"part1 = $part1") // 21213
  println(s"part2 = $part2") // 8549735

}
