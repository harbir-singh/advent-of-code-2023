import scala.annotation.tailrec
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.io.Source
import scala.language.postfixOps

object Day9 extends App {

  val source = Source.fromFile("src/main/resources/Day9.input")
  val input: List[String] = source.getLines.toList
  source.close()

  trait Direction
  case object Next extends Direction
  case object Prev extends Direction

  def inSeq(seq: Seq[Long], direction: Direction): Long = {
    def diffSeq: Seq[Long] =
      seq.tail.zipWithIndex.map {
        case (v, 0) => v - seq.head
        case (v, i) => v - seq(i)
      }
    if (seq.forall(_ == 0L)) 0L
    else {
      direction match {
        case Next => seq.last + inSeq(diffSeq, direction)
        case Prev => seq.head - inSeq(diffSeq, direction)
      }
    }
  }

  def parseInput(inp: List[String]): List[Seq[Long]] = inp.map(_.split(" ").map(_.toLong))

  def part1: Long = parseInput(input).map(inSeq(_, Next)).sum

  def part2: Long = {
    for {
      seq <- parseInput(input)
    } yield inSeq(seq, Prev)
  }.sum

  println(s"part1=$part1") // 1938800261
  println(s"part2=$part2") // 1112
}
