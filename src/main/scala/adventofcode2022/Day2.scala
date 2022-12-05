package adventofcode2022

object Day2 extends Day {

  val R = "R"
  val P = "P"
  val S = "S"

  override def fileName: String = "day2"

  override def puzzle1(input: List[String]): Any =
    makePairs(input)
      .map({
        case (a, b) => getScore(convertOpponent(a), convertSelf1(b))
      })
      .sum

  def convertSelf1(x: String): String =
    x match {
      case "X" => R
      case "Y" => P
      case "Z" => S
    }

  override def puzzle2(input: List[String]): Any =
    makePairs(input)
      .map({
        case (a, b) =>
          val x = convertOpponent(a)
          val y = convertSelf2(x, b)
          getScore(x, y)
      })
      .sum

  def makePairs(lines: List[String]): List[(String, String)] =
    lines.flatMap(_.split(" ").toList match {
      case a :: b :: Nil => Some((a, b))
      case _             => None
    })

  def convertOpponent(x: String): String =
    x match {
      case "A" => R
      case "B" => P
      case "C" => S
    }

  def getScore(a: String, b: String): Int = {
    val choice = b match {
      case R => 1
      case P => 2
      case S => 3
    }
    val score = (a, b) match {
      case (R, P)           => 6
      case (P, S)           => 6
      case (S, R)           => 6
      case (i, j) if i == j => 3
      case _                => 0
    }
    choice + score
  }

  def convertSelf2(a: String, b: String): String =
    b match {
      case "X" => getLoser(a)
      case "Y" => a
      case "Z" => getWinner(a)
    }

  def getLoser(x: String): String =
    strategies(x)(0)

  def strategies: Map[String, List[String]] =
    Map(R -> List(S, P), P -> List(R, S), S -> List(P, R))

  def getWinner(x: String): String =
    strategies(x)(1)

  override protected def testInputStr: String = """A Y
                                                  |B X
                                                  |C Z""".stripMargin
}
