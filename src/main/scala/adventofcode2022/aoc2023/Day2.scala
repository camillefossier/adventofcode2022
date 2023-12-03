package adventofcode2022.aoc2023

import adventofcode2022.Day

object Day2 extends Day {
  override def fileName: String = "2023/day2"

  override def puzzle1(input: List[String]): Any = {
    val bag = Hand(12, 13, 14)
    val games = input.flatMap(parseGame).zipWithIndex
    games.collect({
      case (game, i) if game.possible(bag) => i + 1
    }).sum
  }

  override def puzzle2(input: List[String]): Any =
    input.flatMap(parseGame).map(_.minHand.power).sum

  def parseGame(line: String): Option[Game] =
    for {
      g <- line.split(":").lastOption
      hands = g.split(";").toList.map(parseHand)
    } yield Game(hands)

  def parseHand(str: String): Hand =
    str.split(",").toList.foldLeft(Hand())({
      case (acc, curr) =>
        curr.split(" ").toList match {
          case "" :: nb :: color :: Nil =>
            if (color == "red") acc.copy(red = Integer.parseInt(nb))
            else if (color == "green") acc.copy(green = Integer.parseInt(nb))
            else if (color == "blue") acc.copy(blue = Integer.parseInt(nb))
            else acc
          case _ => acc
        }
    })

  override protected def testInputStr1: String = """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
                                                   |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
                                                   |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
                                                   |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
                                                   |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".stripMargin
}

case class Hand(red: Int = 0, green: Int = 0, blue: Int = 0) {
  def <=(h: Hand): Boolean =
    red <= h.red && green <= h.green && blue <= h.blue

  def max(h: Hand): Hand =
    Hand(m(red, h.red), m(green, h.green), m(blue, h.blue))

  def m(i: Int, j: Int): Int =
    if (i > j) i
    else j

  def power: Int =
    red * green * blue
}
case class Game(hands: List[Hand]) {
  def possible(bag: Hand): Boolean =
    hands.forall(_ <= bag)

  def minHand: Hand =
    hands.reduce(_.max(_))
}
