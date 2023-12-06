import scala.io.Source

object Day2 extends App {

  private type GameId = Int
  private type Game = String

  trait Cube {
    val count: Int
    private val totalRed = 12
    private val totalBlue = 14
    private val totalGreen = 13

    def isValidGame: Boolean = this match {
      case Red(c)   => c <= totalRed
      case Blue(c)  => c <= totalBlue
      case Green(c) => c <= totalGreen
    }

    private def minRequired(cubes: List[Cube]): Cube = this match {
      case Red(c) =>
        cubes.collectFirst { case cube: Red if cube.count > c => cube }.getOrElse(Red(c))
      case Blue(c) =>
        cubes.collectFirst { case cube: Blue if cube.count > c => cube }.getOrElse(Blue(c))
      case Green(c) =>
        cubes.collectFirst { case cube: Green if cube.count > c => cube }.getOrElse(Green(c))
    }

  }

  private object Cube {

    def merge(a: List[Cube], b: List[Cube]): List[Cube] =
      (a.map(_.minRequired(b)) ++ b.map(_.minRequired(a))).distinct

  }

  private case class Red(count: Int) extends Cube
  private case class Blue(count: Int) extends Cube
  private case class Green(count: Int) extends Cube

  val input: List[String] =
    Source.fromFile("src/main/resources/Day2.input").getLines.toList

  def parseRow(row: String): (GameId, List[Game]) =
    row match {
      case s"Game $id: $games" => (id.toInt, games.split("; ").toList)
    }

  private def parseGame(game: Game): List[Cube] = {
    def parseCube(s: String): Cube = s match {
      case s"$num red"   => Red(num.toInt)
      case s"$num blue"  => Blue(num.toInt)
      case s"$num green" => Green(num.toInt)
    }
    game.split(", ").toList.map(parseCube)
  }

  private val part1 =
    input
      .map(parseRow)
      .map { case (id, games) =>
        id -> games.map(game => parseGame(game).forall(_.isValidGame)).forall(_ == true)
      }
      .filter(_._2)
      .map(_._1)
      .sum

  println(s"part1= $part1") // 2913

  private val part2 =
    input
      .map(parseRow)
      .map { case (_, games) =>
        games
          .map(parseGame)
          .reduce(Cube.merge)
          .map(_.count)
          .product
      }
      .sum

  println(s"part2= $part2") // 55593

}
