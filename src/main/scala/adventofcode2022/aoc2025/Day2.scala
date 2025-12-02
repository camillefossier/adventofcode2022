package adventofcode2022.aoc2025

import adventofcode2022.Day
import adventofcode2022.Utils.RichString

object Day2 extends Day {
  override def fileName: String = "2025/day2"

  override def puzzle1(input: List[String]): Any = parseInput(input.head).flatMap(_.invalids).sum

  override def puzzle2(input: List[String]): Any = parseInput(input.head).flatMap(_.invalids2).sum

  override protected def testInputStr1: String =
    "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

  def parseInput(input: String): List[Interval] =
    input
      .split(",")
      .toList
      .map(_.split("-").toList match {
        case List(start, end) => Interval(start.toLong, end.toLong)
      })

  def firstInvalid(nb: Long): Long = {
    val nbDigits = nb.toString.length
    if (nbDigits % 2 == 1)
      s"${Math.pow(10, (nbDigits + 1) / 2 - 1).toLong}${Math.pow(10, (nbDigits + 1) / 2 - 1).toLong}".toLong
    else {
      val a = nb.toString.substring(0, nbDigits / 2).toLong
      val b = s"$a$a".toLong
      if (b < nb) nextInvalid(b) else b
    }
  }

  def nextInvalid(invalid: Long): Long = {
    val nbDigits = invalid.toString.length
    val a =
      if (nbDigits > 1) invalid.toString.substring(0, nbDigits / 2).toLong
      else invalid
    s"${a + 1}${a + 1}".toLong
  }

  def isInvalid2(nbString: String, base: Int): Boolean =
    if (nbString.length % base != 0) false
    else {
      val splitted = nbString.splitEvery(nbString.length / base)
      val head = splitted.head
      splitted.tail.forall(_ == head)
    }

  def isInvalid2(digit: Long): Boolean = {
    val digitStr = digit.toString
    val limit = digitStr.length
    Iterator.iterate(2)(_ + 1).takeWhile(_ <= limit).exists(base => isInvalid2(digit.toString, base))
  }

  case class Interval(start: Long, end: Long) {
    def all: Iterator[Long] = Iterator.iterate(start)(_ + 1).takeWhile(_ <= end)
    def invalids: List[Long] = {
      val result = Iterator.iterate(firstInvalid(start))(nextInvalid).takeWhile(_ <= end).toList
      result
    }

    def invalids2: List[Long] = {
      val res = all.filter(isInvalid2).toList
      res
    }

  }
}
