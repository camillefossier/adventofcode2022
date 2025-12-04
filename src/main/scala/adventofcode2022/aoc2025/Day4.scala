package adventofcode2022.aoc2025

import adventofcode2022.Day

object Day4 extends Day {
  override def fileName: String = "2025/day4"

  override def puzzle1(input: List[String]): Any = {
    val rolls = input.zipWithIndex.flatMap({
      case (line, y) =>
        line.zipWithIndex.flatMap({
          case (_, x) => parse(input, x, y)
        })
    })
    rolls.count(_.countAround < 4)
  }

  override def puzzle2(input: List[String]): Any = countAndUpdate(input)

  override protected def testInputStr1: String = """..@@.@@@@.
                                                   |@@@.@.@.@@
                                                   |@@@@@.@.@@
                                                   |@.@@@@..@.
                                                   |@@.@@@@.@@
                                                   |.@@@@@@@.@
                                                   |.@.@.@.@@@
                                                   |@.@@@.@@@@
                                                   |.@@@@@@@@.
                                                   |@.@.@@@.@.""".stripMargin

  def parse(input: List[String], x: Int, y: Int): Option[Roll] =
    for {
      line <- input.lift(y)
      char <- line.lift(x)
      if char == '@'
      countAround = countSurroundingRolls(input, x, y)
    } yield Roll(x, y, countAround)

  def countSurroundingRolls(input: List[String], x: Int, y: Int): Int =
    (for {
      a <- (x - 1) to (x + 1)
      b <- (y - 1) to (y + 1)
      if !(a == x && b == y)
      char <- input.lift(b).flatMap(line => line.lift(a))
      if char == '@'
    } yield 1).sum

  case class Roll(x: Int, y: Int, countAround: Int)

  def removeRoll(input: List[String], roll: Roll): List[String] =
    input.updated(roll.y, input(roll.y).updated(roll.x, '.'))

  def countAndUpdate(input: List[String], acc: Int = 0): Int = {
    val toRemove = input.zipWithIndex
      .flatMap({
        case (line, y) =>
          line.zipWithIndex.flatMap({
            case (_, x) => parse(input, x, y)
          })
      })
      .filter(_.countAround < 4)

    if (toRemove.isEmpty) acc
    else {
      val updatedInput = toRemove.foldLeft(input)(removeRoll)
      countAndUpdate(updatedInput, acc + toRemove.size)
    }
  }
}
