package adventofcode2022

object Day7 extends Day {

  case class File(name: String, prefix: String, size: Int)

  def getFiles(lines: List[String]): List[File] = {
    val absoluteCdRegex = "^\\$ cd /(.*)$".r("dir")
    val relativeCdRegex = "^\\$ cd (.*)$".r("dir")
    val lsRegex = "^\\$ ls$".r
    val fileRegex = "^([0-9]+) (.*)$".r("size", "filename")
    val dirRegex = "^dir (.*)$".r("directory")
    var prefix = ""
    lines
      .collect({
        case absoluteCdRegex(dir) =>
          prefix = s"$dir"
          None
        case relativeCdRegex(dir) =>
          if (dir == "..")
            prefix = prefix
              .split("/")
              .dropRight(1)
              .mkString("/")
          else
            prefix = s"$prefix/$dir"
          None
        case fileRegex(size, name) => Some(File(name, prefix, size.toInt))
      })
      .flatten
  }

  def getSizes(files: List[File]): Map[String, Int] = {
    val sizes = files.foldLeft(Map.empty[String, Int])({
      case (map, file) =>
        val splitted = file.prefix.split("/")
        val prefixParts = 0
          .to(splitted.size)
          .toList
          .map(i => splitted.slice(1, i).mkString("/"))
        prefixParts.foldLeft(map)(
          (acc, cur) => acc + (cur -> (acc.getOrElse(cur, 0) + file.size))
        )
    })
    // FIXME : BUG! For some reason the root's size is twice as big as it should
    //  We're obviously adding the size twice.
    sizes + ("" -> sizes.get("").map(_ / 2).getOrElse(0))
  }

  override def fileName: String = "day7"

  override def puzzle1(input: List[String]): Any =
    getSizes(getFiles(input)).filter({ case (k, v) => v <= 100000 }).values.sum

  override def puzzle2(input: List[String]): Any = {
    val sizes = getSizes(getFiles(input))
    val totalSize = 70000000
    val requiredFreeSpace = 30000000
    val currentFreeSpace = totalSize - sizes.getOrElse("", 0)
    val toDelete = requiredFreeSpace - currentFreeSpace
    sizes.toList
      .sortBy({ case (dir, size) => size })
      .collectFirst({ case (dir, size) if size >= toDelete => size })
  }

  override protected def testInputStr: String = """$ cd /
                                                  |$ ls
                                                  |dir a
                                                  |14848514 b.txt
                                                  |8504156 c.dat
                                                  |dir d
                                                  |$ cd a
                                                  |$ ls
                                                  |dir e
                                                  |29116 f
                                                  |2557 g
                                                  |62596 h.lst
                                                  |$ cd e
                                                  |$ ls
                                                  |584 i
                                                  |$ cd ..
                                                  |$ cd ..
                                                  |$ cd d
                                                  |$ ls
                                                  |4060174 j
                                                  |8033020 d.log
                                                  |5626152 d.ext
                                                  |7214296 k""".stripMargin
}
