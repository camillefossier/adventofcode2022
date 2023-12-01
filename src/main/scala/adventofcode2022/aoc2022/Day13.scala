package adventofcode2022.aoc2022

import adventofcode2022.Day
import io.circe.{Decoder, HCursor}
import io.circe.parser.{parse => jsonParse}

object Day13 extends Day {
  sealed trait Item
  case class IntItem(i: Int) extends Item
  case class ArrayItem(items: List[Item]) extends Item

  implicit val intItemDecoder: Decoder[IntItem] = Decoder.decodeInt.map(IntItem)
  implicit val itemDecoder: Decoder[Item] = (c: HCursor) =>
    intItemDecoder(c) match {
      case Left(_) => arrayItemDecoder(c)
      case e       => e
  }
  implicit val arrayItemDecoder: Decoder[ArrayItem] =
    Decoder.decodeList[Item](itemDecoder).map(ArrayItem)

  override def fileName: String = "2022/day13"

  def parse(input: List[String]) = {
    input
      .filterNot(_.isEmpty)
      .flatMap(line => jsonParse(line).flatMap(_.as[ArrayItem]).toOption)
  }

  def compareItems(a: Item, b: Item): Int = (a, b) match {
    case (IntItem(x), IntItem(y)) => x.compare(y)
    case (ArrayItem(x :: restX), ArrayItem(y :: restY)) =>
      val first = compareItems(x, y)
      if (first == 0) compareItems(ArrayItem(restX), ArrayItem(restY))
      else first
    case (ArrayItem(Nil), ArrayItem(Nil)) => 0
    case (ArrayItem(Nil), _)              => -1
    case (_, ArrayItem(Nil))              => 1
    case (x: IntItem, y: ArrayItem)       => compareItems(ArrayItem(List(x)), y)
    case (x: ArrayItem, y: IntItem)       => compareItems(x, ArrayItem(List(y)))
  }

  override def puzzle1(input: List[String]): Any = {
    val results = parse(input)
      .sliding(2, 2)
      .map({
        case a :: b :: Nil => (a, b)
      })
      .zipWithIndex
      .map({
        case ((a, b), i) => i + 1 -> (compareItems(a, b) <= 0)
      })
    results
      .collect({ case (i, b) if b => i })
      .sum
  }

  override def puzzle2(input: List[String]): Any = {
    val dividersStr = """
                        |[[2]]
                        |[[6]]""".stripMargin.split("\n").toList
    val dividers = parse(dividersStr)
    val sorted =
      parse(input ++ dividersStr)
        .sorted((x: ArrayItem, y: ArrayItem) => compareItems(x, y))
    val indices = sorted.zipWithIndex.collect({
      case (div, i) if (dividers.contains(div)) => i + 1
    })
    indices.product
  }

  override protected def testInputStr1: String = """[1,1,3,1,1]
                                                   |[1,1,5,1,1]
                                                   |
                                                   |[[1],[2,3,4]]
                                                   |[[1],4]
                                                   |
                                                   |[9]
                                                   |[[8,7,6]]
                                                   |
                                                   |[[4,4],4,4]
                                                   |[[4,4],4,4,4]
                                                   |
                                                   |[7,7,7,7]
                                                   |[7,7,7]
                                                   |
                                                   |[]
                                                   |[3]
                                                   |
                                                   |[[[]]]
                                                   |[[]]
                                                   |
                                                   |[1,[2,[3,[4,[5,6,7]]]],8,9]
                                                   |[1,[2,[3,[4,[5,6,0]]]],8,9]""".stripMargin
}
