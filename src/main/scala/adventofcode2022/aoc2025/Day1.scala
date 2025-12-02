package adventofcode2022.aoc2025

import adventofcode2022.Day

object Day1 extends Day {
  override def fileName: String = "2025/day1"

  override def puzzle1(input: List[String]): Any = {
    val init = Position(50)
    val actions = parseInput(input)
    actions
      .foldLeft((init, 0))({
        case ((pos, count), action) => {
          val newPos = pos.move(action)
          if (newPos.value == 0) (newPos, count + 1)
          else (newPos, count)
        }
      })
      ._2
  }

  override def puzzle2(input: List[String]): Any = {
    val init = Position(50)
    val actions = parseInput(input)
    actions
      .foldLeft((init, 0))({
        case ((pos, count), action) => {
          val countToZero = pos.countToZero(action)
          val remainingAfterReachingZero = action.value - countToZero
          val remainingLoops = remainingAfterReachingZero / 100
          val countIncrease = if (action.value >= countToZero) 1 + remainingLoops else 0
          val newPos = pos.move(action)
          (newPos, count + countIncrease)
        }
      })
      ._2
  }

  override protected def testInputStr1: String = """L68
                                                   |L30
                                                   |R48
                                                   |L5
                                                   |R60
                                                   |L55
                                                   |L1
                                                   |L99
                                                   |R14
                                                   |L82""".stripMargin

  def parseInput(input: List[String]): List[Action] =
    input.map(parseAction)

  def parseAction(str: String): Action =
    str.splitAt(1) match {
      case ("L", value) => Action(negative = true, value.toInt)
      case ("R", value) => Action(negative = false, value.toInt)
    }

  case class Position(value: Int) {
    def move(action: Action): Position =
      if (action.negative) Position((value - action.boundedValue + 100) % 100)
      else Position((value + action.boundedValue + 100) % 100)

    def leftCountToZero: Int = value
    def rightCountToZero: Int = 100 - value
    def countToZero(action: Action): Int =
      if (this.value == 0) 100
      else if (action.negative) leftCountToZero
      else rightCountToZero
  }

  case class Action(negative: Boolean, value: Int) {
    def boundedValue: Int = value % 100
  }
}
