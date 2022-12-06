package object adventofcode2022 {
  implicit class RichIterator[T](arr: List[T]) {
    def split(cond: T => Boolean): List[List[T]] = {
      val i = arr.indexWhere(cond)
      val (before, after) = arr.splitAt(i)
      (before, after) match {
        case (Nil, Nil) => List.empty
        case (a, Nil)   => List(a)
        case (Nil, b)   => b.tail.split(cond)
        case (a, b)     => List(a) ++ b.tail.split(cond)
      }
    }

    def allUnique: Boolean =
      arr.toSet.size == arr.size

    def safeTail(n: Int = 1): List[T] =
      arr.slice(n, arr.size)
  }

  implicit class RichString(str: String) {
    def splitEvery(n: Int): List[String] =
      str.splitAt(n) match {
        case (first, "")   => List(first)
        case (first, rest) => List(first) ++ rest.splitEvery(n)
      }
  }
}
