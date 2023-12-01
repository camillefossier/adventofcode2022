package adventofcode2022.aoc2022

import adventofcode2022.Day

object Day11 extends Day {
  case class Monkeys(monkeys: List[Monkey]) {

    def runRound: Monkeys =
      Monkeys(monkeys.unsafeFold((acc, curr) => curr.run(Monkeys(acc)).monkeys))

    def runRounds(n: Int): Monkeys =
      0.until(n).foldLeft(this)((acc, _) => acc.runRound)

    def updateMonkey(i: Int, f: Monkey => Monkey): Monkeys =
      this.copy(monkeys = monkeys.updated(i, f(monkeys(i))))

    def giveMonkey(from: Int, to: Int, item: Int): Monkeys =
      this
        .updateMonkey(from, _.dropOne.inspectOne)
        .updateMonkey(to, _.getItem(item))
  }

  case class Monkey(id: Int,
                    items: List[Int],
                    operation: Int => Int,
                    divider: Int,
                    ifDivisible: Int,
                    elseDivisible: Int,
                    inspected: Int = 0) {

    override def toString: String = s"${id}:\t${items}"

    def dropOne: Monkey =
      this.copy(items = items.drop(1))

    def getItem(item: Int): Monkey =
      this.copy(items = items :+ item)

    def inspectOne: Monkey =
      this.copy(inspected = inspected + 1)

    def runItem(monkeys: Monkeys, item: Int): Monkeys = {
      val newItem = (operation(item) / 3)
      if (newItem % divider == 0)
        monkeys.giveMonkey(id, ifDivisible, newItem)
      else
        monkeys.giveMonkey(id, elseDivisible, newItem)
    }

    def run(monkeys: Monkeys): Monkeys = {
      items.foldLeft(monkeys)((acc, item) => runItem(acc, item))
    }
  }

  def parseMonkey(id: Int, lines: List[String]): Monkey = {
    val nb = "[0-9]+"
    val items = lines(1).drop(18).split(", ").map(_.toInt).toList
    val op = lines(2)(23)
    val opValue = lines(2).drop(25)
    val divider = lines(3).drop(21).toInt
    val ifDivisible = lines(4).drop(29).toInt
    val elseDivisible = lines(5).drop(30).toInt

    val mathOp = op match {
      case '*' =>
        (a: Int, b: Int) =>
          a * b
      case '+' =>
        (a: Int, b: Int) =>
          a + b
    }
    val operation = opValue match {
      case "old" =>
        (i: Int) =>
          mathOp(i, i)
      case n =>
        (i: Int) =>
          mathOp(i, n.toInt)
    }

    Monkey(id, items, operation, divider, ifDivisible, elseDivisible)
  }

  def parseMonkeys(input: List[String]): Monkeys =
    Monkeys(
      input
        .sliding(7, 7)
        .toList
        .zipWithIndex
        .map({
          case (lines, i) => parseMonkey(i, lines)
        })
    )

  override def fileName: String = "2022/day11"

  override def puzzle1(input: List[String]): Any = {
    parseMonkeys(input)
      .runRounds(20)
      .monkeys
      .map(_.inspected)
      .sorted
      .reverse
      .take(2)
      .product
  }

  override def puzzle2(input: List[String]): Any =
    parseMonkeys(input)
      .runRounds(10000)
      .monkeys
      .map(_.inspected)
      .sorted
      .reverse
      .take(2)
      .product

  override protected def testInputStr1: String =
    """Monkey 0:
                                                   |  Starting items: 79, 98
                                                   |  Operation: new = old * 19
                                                   |  Test: divisible by 23
                                                   |    If true: throw to monkey 2
                                                   |    If false: throw to monkey 3
                                                   |
                                                   |Monkey 1:
                                                   |  Starting items: 54, 65, 75, 74
                                                   |  Operation: new = old + 6
                                                   |  Test: divisible by 19
                                                   |    If true: throw to monkey 2
                                                   |    If false: throw to monkey 0
                                                   |
                                                   |Monkey 2:
                                                   |  Starting items: 79, 60, 97
                                                   |  Operation: new = old * old
                                                   |  Test: divisible by 13
                                                   |    If true: throw to monkey 1
                                                   |    If false: throw to monkey 3
                                                   |
                                                   |Monkey 3:
                                                   |  Starting items: 74
                                                   |  Operation: new = old + 3
                                                   |  Test: divisible by 17
                                                   |    If true: throw to monkey 0
                                                   |    If false: throw to monkey 1""".stripMargin
}
