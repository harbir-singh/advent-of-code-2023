import scala.annotation.tailrec
import scala.io.Source

object Day1 extends App {

  private val input: List[String] =
    Source.fromFile("src/main/resources/Day1.input").getLines().toList

  private def parseRow(s: String): Int =
    (s.find(_.isDigit).fold(0)(_.asDigit) * 10) + s.findLast(_.isDigit).fold(0)(_.asDigit)

  println(s"part1 = ${input.map(parseRow).sum}")

  private def parseRow2(str: String): Int = {
    val s = str.toLowerCase
    def digitIndices(s: String): List[(Char, Int)] = s.zipWithIndex.toList.collect {
      case (c, i) if c.isDigit => c -> i
    }
    def wordDigitIndices(s: String): List[(Char, Int)] = {
      @tailrec
      def allIndexOf(
        str: String,
        key: String,
        index: Int = 0,
        acc: List[Int] = List.empty,
      ): List[Int] = {
        val idx = str.indexOf(key, index)
        if (idx == -1)
          acc
        else
          allIndexOf(str, key, index + idx + key.length, acc :+ idx)
      }

      val possibilities: List[(String, Char)] = List(
        "one" -> '1',
        "two" -> '2',
        "three" -> '3',
        "four" -> '4',
        "five" -> '5',
        "six" -> '6',
        "seven" -> '7',
        "eight" -> '8',
        "nine" -> '9',
      )

      possibilities.flatMap { case (k, v) => allIndexOf(s, k).map(v -> _) }
    }
    def result(allIndicesSorted: List[(Char, Int)]): Int = {
      val tens: Int = allIndicesSorted.headOption.fold('0')(_._1).asDigit * 10
      val ones: Int = allIndicesSorted.lastOption.fold('0')(_._1).asDigit
      tens + ones
    }
    val allIndicesSorted = (digitIndices(s) ++ wordDigitIndices(s)).sortWith { case (a, b) =>
      a._2 < b._2
    }
    result(allIndicesSorted)
  }

//  println(parseRow2("two1nine"))
//  println(parseRow2("eightwothree"))
//  println(parseRow2("abcone2threexyz"))
//  println(parseRow2("xtwone3four"))
//  println(parseRow2("4nineeightseven2"))
//  println(parseRow2("zoneight234"))
//  println(parseRow2("7pqrstsixteen"))
//  println(parseRow2("asjkfhasjh"))
//  println(parseRow2("as1jkfhasjh"))
//  println(parseRow2("zoneight"))
//  println(parseRow2("pseven3threeeightseven"))

  println(s"part2 = ${input.map(parseRow2).sum}")

}
