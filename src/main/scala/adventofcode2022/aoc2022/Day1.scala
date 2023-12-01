package adventofcode2022.aoc2022

import adventofcode2022.Day

object Day1 extends Day {

  override def fileName: String = "2022/day1"

  override def puzzle1(input: List[String]): Any = {
    val calories = sortedCaloriesByElf(input)
    topElves(calories, 1)
  }

  override def puzzle2(input: List[String]): Any = {
    val calories = sortedCaloriesByElf(input)
    topElves(calories, 3)
  }

  def sortedCaloriesByElf(input: List[String]): List[Int] =
    input
      .split(_.isEmpty)
      .map(_.map(_.toInt).sum)
      .sorted
      .reverse

  def topElves(list: List[Int], n: Int): Int =
    list.slice(0, n).sum

  override protected def testInputStr1: String = """1000
                                                  |2000
                                                  |3000
                                                  |
                                                  |4000
                                                  |
                                                  |5000
                                                  |6000
                                                  |
                                                  |7000
                                                  |8000
                                                  |9000
                                                  |
                                                  |10000""".stripMargin
}
