package adventofcode2022

object Day8 extends Day {

  type Forest = List[List[Int]]

  override def fileName: String = "day8"

  def getForestVisibleTreesCount(forest: Forest): Int = {
    val l = forest.size
    forest.zipWithIndex
      .flatMap({
        case (row, y) =>
          val n = row.size
          row.zipWithIndex.filter({
            case (tree, x) =>
              row
                .slice(0, x)
                .forall(_ < tree) || row.slice(x + 1, n).forall(_ < tree) ||
                forest.slice(0, y).map(_(x)).forall(_ < tree) || forest
                .slice(y + 1, l)
                .map(_(x))
                .forall(_ < tree)
          })
      })
      .size
  }

  def getTreeScore(forest: Forest,
                   row: List[Int],
                   height: Int,
                   width: Int,
                   tree: Int,
                   x: Int,
                   y: Int): Int = {
    def count(list: List[Int], limit: Int): Int = {
      val res = list.takeWhile(_ < limit).size
      if (res < list.size) res + 1 else res
    }

    val north = count(forest.slice(0, y).map(_(x)).reverse, tree)
    val south = count(forest.slice(y + 1, height).map(_(x)), tree)
    val west = count(row.slice(0, x).reverse, tree)
    val east = count(row.slice(x + 1, width), tree)

    north * south * west * east
  }

  def getForestMatrix(input: List[String]): Forest =
    input.map(_.split("").map(_.toInt).toList)

  override def puzzle1(input: List[String]): Any = {
    val forest = getForestMatrix(input)
    getForestVisibleTreesCount(forest)
  }

  override def puzzle2(input: List[String]): Any = {
    val forest = getForestMatrix(input)
    val height = forest.size
    forest.zipWithIndex
      .flatMap({
        case (row, y) =>
          val width = row.size
          row.zipWithIndex
            .map({
              case (tree, x) =>
                getTreeScore(forest, row, height, width, tree, x, y)
            })
      })
      .max
  }

  override protected def testInputStr: String = """30373
                                                  |25512
                                                  |65332
                                                  |33549
                                                  |35390""".stripMargin
}
