package adventofcode2022.aoc2023

import adventofcode2022.Day

object Day4 extends Day {
  override def fileName: String = "2023/day4"

  override def puzzle1(input: List[String]): Any = {
    input.flatMap(parse).map(_.score).sum
  }

  def parse(line: String): Option[Card] = {
    for {
      nbs <- line.split(":").lastOption
      split = nbs.split("\\|").toList
      winning <- split.headOption.map(a => "([0-9]+)".r.findAllIn(a).toList)
      self <- split.lastOption.map(a => "([0-9]+)".r.findAllIn(a).toList)
    } yield
      Card(
        winning.toSet.map(Integer.parseInt),
        self.toSet.map(Integer.parseInt)
      )
  }

  override def puzzle2(input: List[String]): Any = {
    val initialCards = input.flatMap(parse).zipWithIndex
    scoreRecursive(initialCards, initialCards)
  }

  def scoreRecursive(input: List[(Card, Int)], cards: List[(Card, Int)]): Int =
    cards.foldLeft(0)({
      case (acc, (card, i)) =>
        val cardsToCopy = input.slice(i + 1, i + 1 + card.nbWinning)
        acc + 1 + scoreRecursive(input, cardsToCopy)
    })

  override protected def testInputStr1: String = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
                                                   |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
                                                   |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
                                                   |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
                                                   |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
                                                   |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".stripMargin

  case class Card(winning: Set[Int], self: Set[Int]) {
    val nbWinning: Int = winning.intersect(self).size

    def score: Int = {
      if (nbWinning == 0) 0
      else Math.pow(2, nbWinning - 1).toInt
    }
  }
}
