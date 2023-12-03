package adventofcode2022

import scala.annotation.tailrec

object Utils {
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

    def unsafeFold(f: (List[T], T) => List[T]): List[T] =
      arr.indices.foldLeft(arr)((acc, i) => f(acc, acc(i)))
  }

  implicit class RichString(str: String) {
    def splitEvery(n: Int): List[String] =
      str.sliding(n, n).toList

    def splitNoEmpty(sep: String): Array[String] =
      str.split(sep).filter(_.nonEmpty)
  }

  case class Point2(x: Int, y: Int) {
    def +(other: Point2): Point2 =
      Point2(x + other.x, y + other.y)

    def +(x: Int = 0, y: Int = 0): Point2 =
      this + Point2(x, y)
  }
  case class Point3(x: Int, y: Int, z: Int) {
    def +(other: Point3): Point3 =
      Point3(x + other.x, y + other.y, z + other.z)
  }

  @tailrec
  def foldWhileTrue[T](init: T)(f: (T) => (T, Boolean)): T =
    f(init) match {
      case (t, true)  => foldWhileTrue(t)(f)
      case (t, false) => t
    }
}
