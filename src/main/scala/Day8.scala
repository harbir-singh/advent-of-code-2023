import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

object Day8 extends App {

  val source = Source.fromFile("src/main/resources/Day8.input")
  val input: List[String] = source.getLines.toList
  source.close()

  def inputParser(rows: List[String]): (List[String], Map[String, (String, String)]) = {
    @tailrec
    def aux(
      rows: List[String],
      map: Map[String, (String, String)] = Map.empty,
    ): Map[String, (String, String)] =
      rows match {
        case row :: tail =>
          row match {
            case s"$key = ($left, $right)" => aux(tail, map.updated(key, left -> right))
          }
        case Nil => map
      }

    val instructions = rows.head.split("").toList
    val map = aux(rows.tail)
    instructions -> map
  }

  def part1(start: String = "AAA", predicate: String => Boolean = _ == "ZZZ"): Long = {
    val (ins, lrMap) = inputParser(input.filterNot(_.isBlank))

    @tailrec
    def aux(
      current: String,
      index: Int = 0,
      numSteps: Long = 0,
    ): Long =
      if (predicate(current)) numSteps
      else {
        ins(index) match {
          case "L" => aux(lrMap(current)._1, (index + 1) % ins.size, numSteps + 1L)
          case "R" => aux(lrMap(current)._2, (index + 1) % ins.size, numSteps + 1L)
        }
      }

    aux(start)
  }

  def part2: BigInt = {
    val (_, lrMap) = inputParser(input.filterNot(_.isBlank))

    lrMap.keys.toList
      .filter(_.endsWith("A"))
      .map(key => part1(key, _.endsWith("Z")))
      .foldLeft(1: BigInt)((acc, v) => (acc * v) / acc.gcd(v))
  }

  println(s"part1=${part1()}") // 13207
  println(s"part2=$part2") // 12324145107121
}
