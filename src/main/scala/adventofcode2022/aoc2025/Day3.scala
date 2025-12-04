package adventofcode2022.aoc2025

import adventofcode2022.Day

object Day3 extends Day {
  override def fileName: String = "2025/day3"

  override def puzzle1(input: List[String]): Any = input.map(getJoltage(_, 2)).sum

  override def puzzle2(input: List[String]): Any = input.map(getJoltage(_, 12)).sum

  override protected def testInputStr1: String =
    """987654321111111
      |811111111111119
      |234234234234278
      |818181911112111""".stripMargin

  def getJoltage(input: String, n: Int): Long = {
    val l = input.split("").toList
    val res = 0
      .to(n - 1)
      .reverse
      .foldLeft((l, ""))({
        case ((line, acc), i) =>
          val max = line.slice(0, line.length - i).max
          val nextRemaining = line.drop(line.indexOf(max) + 1)
          (nextRemaining, acc ++ max)
      })
      ._2
      .toLong
    res
  }

}
