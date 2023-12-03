import scala.annotation.tailrec
import scala.io.Source

object Day3 extends App {

  val source = Source.fromFile("src/main/resources/Day3.input")
  val input: List[String] = source.getLines.toList
  // indices without first and last rows
  val rowIndices: List[Int] = (1 to input.size - 2).toList

  def getPartNumber(row: String, approxIndex: Int): Int = {
    @tailrec
    def startIndex(si: Int): Int =
      if (row.charAt(si).isDigit) {
        if (si == 0)
          0
        else
          startIndex(si - 1)
      } else
        si + 1
    @tailrec
    def endIndex(si: Int): Int =
      if (si == row.length)
        si
      else if (row.charAt(si).isDigit)
        endIndex(si + 1)
      else
        si

    val s = startIndex(approxIndex)
    val e = endIndex(approxIndex)
    if (s < e) row.substring(s, e).toInt else 0
  }

  def isSpecialChar(c: Char): Boolean =
    if (c.isLetterOrDigit || c == '.')
      false
    else
      true

  // looking for specialCharacters and returning the adjacent partNumbers
  def parseRows_part1(rowIndex: Int): Set[(Int, Int, Int)] = {
    val prevRow = input(rowIndex - 1)
    val currRow = input(rowIndex)
    val nextRow = input(rowIndex + 1)
    currRow.zipWithIndex.flatMap { case (c, i) =>
      if (isSpecialChar(c)) {
        // previous row
        val r1l = getPartNumber(prevRow, i - 1)
        val r1c = getPartNumber(prevRow, i)
        val r1r = getPartNumber(prevRow, i + 1)
        // current row
        val r2l = getPartNumber(currRow, i - 1)
        val r2r = getPartNumber(currRow, i + 1)
        // next row
        val r3l = getPartNumber(nextRow, i - 1)
        val r3c = getPartNumber(nextRow, i)
        val r3r = getPartNumber(nextRow, i + 1)
        Set( // save row anc col index to distinguish duplicate part numbers
          (r1l, rowIndex, i),
          (r1c, rowIndex, i),
          (r1r, rowIndex, i),
          (r2l, rowIndex, i),
          (r2r, rowIndex, i),
          (r3l, rowIndex, i),
          (r3c, rowIndex, i),
          (r3r, rowIndex, i),
        ).filterNot(_._1 == 0)
      } else
        Set.empty
    }.toSet
  }

  // looking for gear ('*' with exactly 2 adjacent partNumbers) and computing its product
  def parseRows_part2(rowIndex: Int): Set[Int] = {
    val prevRow = input(rowIndex - 1)
    val currRow = input(rowIndex)
    val nextRow = input(rowIndex + 1)
    currRow.zipWithIndex.map { case (c, i) =>
      if (c == '*') {
        // previous row
        val r1l = getPartNumber(prevRow, i - 1)
        val r1c = getPartNumber(prevRow, i)
        val r1r = getPartNumber(prevRow, i + 1)
        // current row
        val r2l = getPartNumber(currRow, i - 1)
        val r2r = getPartNumber(currRow, i + 1)
        // next row
        val r3l = getPartNumber(nextRow, i - 1)
        val r3c = getPartNumber(nextRow, i)
        val r3r = getPartNumber(nextRow, i + 1)
        val s: Set[(Int, Int, Int)] =
          Set(
            (r1l, rowIndex, i),
            (r1c, rowIndex, i),
            (r1r, rowIndex, i),
            (r2l, rowIndex, i),
            (r2r, rowIndex, i),
            (r3l, rowIndex, i),
            (r3c, rowIndex, i),
            (r3r, rowIndex, i),
          ).filterNot(_._1 == 0)
        if (s.size == 2)
          s.map(_._1).product
        else
          0
      } else 0
    }.toSet
  }

  println(s"part1 = ${rowIndices.flatMap(parseRows_part1).map(_._1).sum}") // 550934
  println(s"part2 = ${rowIndices.flatMap(parseRows_part2).sum}") // 81997870

  def part1: Int = {
    for {
      ri <- rowIndices
      partNumbersTuple <- parseRows_part1(ri)
    } yield partNumbersTuple
  }.map(_._1).sum

  println(part1)

  source.close()
}
