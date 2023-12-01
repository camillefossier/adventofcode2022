package adventofcode2022.aoc2023

import adventofcode2022.Day

object Day1 extends Day {

  val numbersMap = Map(
    "one" -> "1",
    "two" -> "2",
    "three" -> "3",
    "four" -> "4",
    "five" -> "5",
    "six" -> "6",
    "seven" -> "7",
    "eight" -> "8",
    "nine" -> "9"
  )

  override def fileName: String = "2023/day1"

  override def puzzle1(input: List[String]): Any = {
    val pattern = "([0-9])".r
    input
      .flatMap(line => {
        val m = pattern.findAllIn(line).toList
        for {
          h <- m.headOption
          t <- m.lastOption
        } yield Integer.parseInt(h ++ t)
      })
      .sum
  }

  override def puzzle2(input: List[String]): Any = {
    val pattern = "(?=([0-9]|one|two|three|four|five|six|seven|eight|nine))".r
    input
      .flatMap(line => {
        val m = pattern.findAllIn(line).matchData.map(m => m.group(1)).toList
        for {
          h <- m.headOption
          t <- m.lastOption
          hn = numbersMap.getOrElse(h, h)
          tn = numbersMap.getOrElse(t, t)
        } yield Integer.parseInt(hn ++ tn)
      })
      .sum
  }

  override protected def testInputStr1: String = """1abc2
                                                   |pqr3stu8vwx
                                                   |a1b2c3d4e5f
                                                   |treb7uchet""".stripMargin

  override protected def testInputStr2: String = """two1nine
                                                   |eightwothree
                                                   |abcone2threexyz
                                                   |xtwone3four
                                                   |4nineeightseven2
                                                   |zoneight234
                                                   |7pqrstsixteen""".stripMargin
}
