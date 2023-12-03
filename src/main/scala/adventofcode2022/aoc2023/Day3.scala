package adventofcode2022.aoc2023

import adventofcode2022.Day
import adventofcode2022.Utils._

object Day3 extends Day {
  override def fileName: String = "2023/day3"

  override def puzzle1(input: List[String]): Any =
    parse(input).flatMap(_.numbers).map(_.number).sum

  def parse(input: List[String]): List[ConnectedNumbers] =
    input.zipWithIndex.map({
      case (line, y) =>
        line.zipWithIndex
          .foldLeft(ConnectedNumbers(List.empty, "", Set.empty))({
            case (acc, (char, x)) =>
              val specialAround = findSpecialAround(input, x, y)
              if (isNumber(char)) acc.copy(currentNumber = acc.currentNumber + char, special = acc.special ++ specialAround)
              else acc.flush
          }).flush
    })

  def findSpecialAround(input: List[String], x: Int, y: Int): Set[Special] =
    (for {
      i <- (x - 1) to (x + 1)
      j <- (y - 1) to (y + 1)
      k <- input.lift(j).flatMap(_.lift(i))
      if isSpecial(k)
    } yield Special(k, Point2(i, j))).toSet

  def isSpecial(char: Char): Boolean = !isNumber(char) && !isDot(char)

  def isDot(char: Char): Boolean = char == '.'

  def isNumber(char: Char): Boolean =
    Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9').contains(char)

  override def puzzle2(input: List[String]): Any =
    parse(input)
      .flatMap(_.numbers)
      .flatMap(n => n.specials.map(n -> _))
      .groupBy({ case (_, s) => s})
      .collect({
        case (s, nbs) if s.char == '*' && nbs.size == 2 =>
          nbs.map(_._1.number).product
      })
      .sum

  override protected def testInputStr1: String = """467..114..
                                                   |...*......
                                                   |..35..633.
                                                   |......#...
                                                   |617*......
                                                   |.....+.58.
                                                   |..592.....
                                                   |......755.
                                                   |...$.*....
                                                   |.664.598..""".stripMargin

  case class ConnectedNumbers(numbers: List[Number],
                              currentNumber: String,
                              special: Set[Special]) {
    def flush: ConnectedNumbers = {
      if (currentNumber.isEmpty) copy(special = Set.empty)
      else if (special.nonEmpty) {
        val number = Number(Integer.parseInt(currentNumber), special)
        copy(
          numbers = numbers :+ number,
          currentNumber = "",
          special = Set.empty
        )
      } else copy(currentNumber = "", special = Set.empty)
    }
  }

}

case class Number(number: Int, specials: Set[Special])
case class Special(char: Char, p: Point2)
