import scala.annotation.tailrec
import scala.collection.immutable.NumericRange
import scala.io.Source
import scala.language.postfixOps

object Day5 extends App {

  val source = Source.fromFile("src/main/resources/Day5.input")
  val input: List[String] = source.getLines.toList
  source.close()

  val keys: List[String] = List(
    "seed-to-soil",
    "soil-to-fertilizer",
    "fertilizer-to-water",
    "water-to-light",
    "light-to-temperature",
    "temperature-to-humidity",
    "humidity-to-location",
  )

  val mapsRaw: Map[String, List[String]] = {
    @tailrec
    def parser(
      rows: List[String],
      index: Int = 0,
      recentKey: String = "",
      result: Map[String, List[String]] = Map.empty,
    ): Map[String, List[String]] =
      if (index < rows.size)
        if (rows(index).head.isLetter) {
          val key = rows(index).split(" ").head.trim
          parser(rows, index + 1, key, result.updated(key, List.empty))
        } else {
          val existing: List[String] = result(recentKey)
          parser(rows, index + 1, recentKey, result.updated(recentKey, existing :+ rows(index)))
        }
      else
        result

    parser(input.tail.filterNot(_.isBlank))
  }

  def part1: Long = {
    def getTheOther(mapKey: String, find: Long): Long = {
      val rows: Seq[String] = mapsRaw(mapKey)

      def lookup(row: String): Option[Long] = row match {
        case s"$d $s $r" =>
          val dest = d.toLong
          val source = s.toLong
          val range = r.toLong
          if ((source until source + range).contains(find))
            Option(dest + (find - source))
          else
            None
      }

      rows.map(lookup).find(_.isDefined).fold(find)(_.get)
    }
    val seeds: List[Long] = input.head match {
      case s"seeds: $s" => s.split(" ").map(_.toLong).toList
    }
    keys.foldLeft(seeds)((acc, key) => acc.map(getTheOther(key, _))).min
  }

  def part2: Long = {
    def seedRanges: List[NumericRange.Inclusive[Long]] = input.head match {
      case s"seeds: $s" =>
        s.split(" ")
          .map(_.toLong)
          .grouped(2)
          .map(sr => sr.head to sr.head + sr.last)
          .toList
    }

    def parseRows(rows: List[String]): List[(NumericRange.Inclusive[Long], Long)] = {
      def parser(row: String): ((Long, Long), Long) = row match {
        case s"$d $s $r" =>
          val dest = d.toLong
          val range = r.toLong
          val source = s.toLong
          ((source, source + range - 1L), source - dest)
      }

      rows.map(parser).map { case (r, o) => (r._1 to r._2, o) }
    }

    def transformRanges(
      sourceRanges: List[NumericRange.Inclusive[Long]],
      that: List[(NumericRange.Inclusive[Long], Long)],
    ): List[NumericRange.Inclusive[Long]] = {
      @tailrec
      def transform(
        sourceRange: NumericRange.Inclusive[Long],
        matches: List[(NumericRange.Inclusive[Long], Long)],
        result: List[NumericRange.Inclusive[Long]] = List.empty,
      ): List[NumericRange.Inclusive[Long]] = {
        val min = sourceRange.min
        val max = sourceRange.max
        matches.sortBy(_._1.min) match {
          case Nil => result
          case (rng, offset) :: tail =>
            val rMin: Long = Math.max(min, rng.min)
            val rMax: Long = Math.min(max, rng.max)
            val endRange = tail.headOption.fold(max)(_._1.min - 1L)
            transform(
              sourceRange,
              tail,
              result ++ List(rMin - offset to rMax - offset, rMax + 1L to endRange)
                .filterNot(_.isEmpty),
            )
        }
      }

      def getRange(num: Long): Option[(NumericRange.Inclusive[Long], Long)] =
        that.find(_._1.contains(num))

      def getRanges(
        sourceRange: NumericRange.Inclusive[Long]
      ): List[(NumericRange.Inclusive[Long], Long)] = {
        def isInBetweenRange(range: NumericRange.Inclusive[Long]): Boolean =
          sourceRange.min < range.min && sourceRange.max > range.max

        val interRanges = that.collect { case ro if isInBetweenRange(ro._1) => ro }
        getRange(sourceRange.min).toList ++ getRange(sourceRange.max).toList ++ interRanges
      }

      sourceRanges.flatMap(sr =>
        getRanges(sr) match {
          case Nil                         => List(sr)
          case matches if matches.nonEmpty => transform(sr, matches)
        }
      )
    }

    keys
      .foldLeft(seedRanges)((acc, key) => transformRanges(acc, parseRows(mapsRaw(key))))
      .map(_.min)
      .min
  }

  println(s"part1 = $part1") // 289863851
  println(s"part2 = ${part2}") // 60568880

}
