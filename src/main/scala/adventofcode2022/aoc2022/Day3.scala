package adventofcode2022.aoc2022

import adventofcode2022.Day
import adventofcode2022.Utils._

object Day3 extends Day {

  override def fileName: String = "2022/day3"

  override def puzzle1(input: List[String]): Any =
    input
      .flatMap(l => getCommonItems(getCompartments(l)).map(getValue))
      .sum

  def getCompartments(string: String): List[String] =
    string.splitAt(string.length / 2) match {
      case (a, b) => List(a, b)
    }

  override def puzzle2(input: List[String]): Any = {
    val n = 3
    input
      .sliding(n, n)
      .toList
      .map(group => getValue(getCommonItems(group).head))
      .sum
  }

  def getValue(item: Char): Int =
    if (item.isLower) item.toInt - 96
    else item.toInt - 38

  def getCommonItems(bags: List[String]): Set[Char] =
    bags
      .safeTail()
      .foldLeft(bags.headOption.fold(Set.empty[Char])(_.toSet))({
        case (a, b) => a.intersect(b.toSet)
      })

  override protected def testInputStr1: String =
    """vJrwpWtwJgWrhcsFMMfFFhFp
      |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
      |PmmdzqPrVvPwwTWBwg
      |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
      |ttgJtRGJQctTZtZT
      |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin
}
