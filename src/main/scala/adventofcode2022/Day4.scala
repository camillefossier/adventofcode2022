package adventofcode2022

object Day4 extends Day {

  override def fileName: String = "day4"

  override def puzzle1(input: List[String]): Any =
    input.count(
      line =>
        parseLine(line) match {
          case a :: b :: Nil => oneIncludeOther(a, b)
      }
    )

  def parseLine(line: String): List[Set[Int]] =
    line
      .split(",")
      .map(_.split("-").toList match {
        case a :: b :: Nil => (a.toInt to b.toInt).toSet
      })
      .toList

  def oneIncludeOther[T](set1: Set[T], set2: Set[T]): Boolean = {
    val (a, b) = if (set1.size >= set2.size) (set1, set2) else (set2, set1)
    a.intersect(b) == b
  }

  override def puzzle2(input: List[String]): Any =
    input.count(
      line =>
        parseLine(line) match {
          case a :: b :: Nil => overlap(a, b)
      }
    )

  def overlap[T](set1: Set[T], set2: Set[T]): Boolean =
    set1.intersect(set2).nonEmpty

  override protected def testInputStr: String = """2-4,6-8
                                                  |2-3,4-5
                                                  |5-7,7-9
                                                  |2-8,3-7
                                                  |6-6,4-6
                                                  |2-6,4-8""".stripMargin
}
