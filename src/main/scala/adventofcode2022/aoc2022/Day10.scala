package adventofcode2022.aoc2022

import adventofcode2022.Day

object Day10 extends Day {
  override def fileName: String = "2022/day10"

  def applyInstruction(input: (Int, Int), line: String): (Int, Int) = {
    val (index, value) = input
    line.split(" ").toList match {
      case "addx" :: delta :: Nil => (index + 2, value + delta.toInt)
      case "noop" :: Nil          => (index + 1, value)
      case _                      => input
    }
  }

  def applyInstructions(lines: List[String]): List[(Int, Int)] =
    lines.foldLeft(List((1, 1)))(
      (acc, curr) => acc :+ applyInstruction(acc.last, curr)
    )

  def findCycles(res: List[(Int, Int)]) =
    res
      .sliding(2)
      .collect({
        case (i, x) :: (j, y) :: Nil
            if (i).until(j).exists(k => k == 20 || (k - 20) % 40 == 0) =>
          (i.until(j).find(k => k == 20 || (k - 20) % 40 == 0).get, x)
      })
      .toList

  def getStrength(list: List[(Int, Int)]): Int =
    list.map({ case (a, b) => a * b }).sum

  override def puzzle1(input: List[String]): Any = {
    getStrength(findCycles(applyInstructions(input)))
  }

  def draw(list: List[(Int, Int)]): String = {
    list
      .sliding(2)
      .toList
      .map({
        case (i, _x) :: (j, _) :: Nil =>
          val line = i / 40
          val x = _x + (40 * line)
          i.until(j)
            .map(k => if ((x).to(x + 2).contains(k)) "#" else " ")
            .mkString("")
      })
      .mkString("")
      .splitEvery(40)
      .mkString("\n")
  }

  override def puzzle2(input: List[String]): Any =
    draw(applyInstructions(input))

  protected def testInputStr: String = """noop
                                                   |addx 3
                                                   |addx -5""".stripMargin

  // 1 + 15 - 11 + 6 - 3 + 5 - 1 - 8 + 13 + 4 = 21.)
  override protected def testInputStr1: String = """addx 15
                                                   |addx -11
                                                   |addx 6
                                                   |addx -3
                                                   |addx 5
                                                   |addx -1
                                                   |addx -8
                                                   |addx 13
                                                   |addx 4
                                                   |noop
                                                   |addx -1
                                                   |addx 5
                                                   |addx -1
                                                   |addx 5
                                                   |addx -1
                                                   |addx 5
                                                   |addx -1
                                                   |addx 5
                                                   |addx -1
                                                   |addx -35
                                                   |addx 1
                                                   |addx 24
                                                   |addx -19
                                                   |addx 1
                                                   |addx 16
                                                   |addx -11
                                                   |noop
                                                   |noop
                                                   |addx 21
                                                   |addx -15
                                                   |noop
                                                   |noop
                                                   |addx -3
                                                   |addx 9
                                                   |addx 1
                                                   |addx -3
                                                   |addx 8
                                                   |addx 1
                                                   |addx 5
                                                   |noop
                                                   |noop
                                                   |noop
                                                   |noop
                                                   |noop
                                                   |addx -36
                                                   |noop
                                                   |addx 1
                                                   |addx 7
                                                   |noop
                                                   |noop
                                                   |noop
                                                   |addx 2
                                                   |addx 6
                                                   |noop
                                                   |noop
                                                   |noop
                                                   |noop
                                                   |noop
                                                   |addx 1
                                                   |noop
                                                   |noop
                                                   |addx 7
                                                   |addx 1
                                                   |noop
                                                   |addx -13
                                                   |addx 13
                                                   |addx 7
                                                   |noop
                                                   |addx 1
                                                   |addx -33
                                                   |noop
                                                   |noop
                                                   |noop
                                                   |addx 2
                                                   |noop
                                                   |noop
                                                   |noop
                                                   |addx 8
                                                   |noop
                                                   |addx -1
                                                   |addx 2
                                                   |addx 1
                                                   |noop
                                                   |addx 17
                                                   |addx -9
                                                   |addx 1
                                                   |addx 1
                                                   |addx -3
                                                   |addx 11
                                                   |noop
                                                   |noop
                                                   |addx 1
                                                   |noop
                                                   |addx 1
                                                   |noop
                                                   |noop
                                                   |addx -13
                                                   |addx -19
                                                   |addx 1
                                                   |addx 3
                                                   |addx 26
                                                   |addx -30
                                                   |addx 12
                                                   |addx -1
                                                   |addx 3
                                                   |addx 1
                                                   |noop
                                                   |noop
                                                   |noop
                                                   |addx -9
                                                   |addx 18
                                                   |addx 1
                                                   |addx 2
                                                   |noop
                                                   |noop
                                                   |addx 9
                                                   |noop
                                                   |noop
                                                   |noop
                                                   |addx -1
                                                   |addx 2
                                                   |addx -37
                                                   |addx 1
                                                   |addx 3
                                                   |noop
                                                   |addx 15
                                                   |addx -21
                                                   |addx 22
                                                   |addx -6
                                                   |addx 1
                                                   |noop
                                                   |addx 2
                                                   |addx 1
                                                   |noop
                                                   |addx -10
                                                   |noop
                                                   |noop
                                                   |addx 20
                                                   |addx 1
                                                   |addx 2
                                                   |addx 2
                                                   |addx -6
                                                   |addx -11
                                                   |noop
                                                   |noop
                                                   |noop""".stripMargin
}
