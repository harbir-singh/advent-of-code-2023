import scala.annotation.tailrec
import scala.collection.immutable.NumericRange
import scala.io.Source
import scala.language.postfixOps

object Day6 extends App {

  val source = Source.fromFile("src/main/resources/Day6.input")
  val input: List[String] = source.getLines.toList
  source.close()

  type Distance = Long
  type Time = Long

  def calculateDistance(buttonHold: Time, raceTime: Time): Distance =
    if (buttonHold < raceTime)
      buttonHold * (raceTime - buttonHold)
    else 0

  @tailrec
  def numWaysToBeatRecord(
    raceTime: Time,
    recordDistance: Distance,
    timeIndex: Long = 1,
  ): Long =
    if (timeIndex < raceTime) {
      val myDistance: Distance = calculateDistance(timeIndex, raceTime)
      if (myDistance > recordDistance)
        (raceTime - 1L) - (2L * (timeIndex - 1L))
      else
        numWaysToBeatRecord(raceTime, recordDistance, timeIndex + 1)
    } else
      0L

  def part1: Long = {
    def inputParser: Map[Time, Distance] = {
      input.map {
        case s"Time: $times"         => times.split(" ").filterNot(_.isBlank).map(_.toLong)
        case s"Distance: $distances" => distances.split(" ").filterNot(_.isBlank).map(_.toLong)
      } match {
        case times :: distance :: Nil => times zip distance
      }
    }.toMap

    inputParser.map { case (rt, rd) =>
      numWaysToBeatRecord(rt, rd)
    }
  }.product

  def part2: Long = {
    def inputParser: Map[Time, Distance] = {
      input.map {
        case s"Time: $times" => times.replace(" ", "").split(" ").filterNot(_.isBlank).map(_.toLong)
        case s"Distance: $distances" =>
          distances.replace(" ", "").split(" ").filterNot(_.isBlank).map(_.toLong)
      } match {
        case times :: distance :: Nil => times zip distance
      }
    }.toMap

    inputParser.map { case (rt, rd) =>
      numWaysToBeatRecord(rt, rd)
    }
  }.product

  println(s"part1=$part1") // 449550
  println(s"part2=$part2") // 28360140
}
