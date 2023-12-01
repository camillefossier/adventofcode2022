package adventofcode2022.aoc2022

import adventofcode2022.Day

object Day6 extends Day {
  override def fileName: String = "2022/day6"

  override def puzzle1(input: List[String]): Any =
    findMarker(input.flatMap(_.toCharArray))

  override def puzzle2(input: List[String]): Any =
    findMarker(input.flatMap(_.toCharArray), 14)

  def findMarker(input: List[Char], size: Int = 4): Int =
    input
      .sliding(size)
      .zipWithIndex
      .collectFirst({
        case (window, i) if window.allUnique => i + size
      })
      .get

  override protected def testInputStr1: String =
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
}
