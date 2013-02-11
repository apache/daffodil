package edu.illinois.ncsa.daffodil.util

object ListUtils {
  type SubListFinder[T] = (Seq[T], Any) => Seq[T]

  def tailAfter[T](lst: Seq[T], valueToFind: Any): Seq[T] = {
    lst match {
      case Nil => Nil
      case hd :: tl => {
        if (hd == valueToFind) tl
        else tailAfter(tl, valueToFind)
      }
    }
  }

  def preceding[T](lst: Seq[T], valueToFind: Any) = {
    val res = preceding1(lst, valueToFind, Nil)
    res
  }

  private def preceding1[T](lst: Seq[T], valueToFind: Any, onto: List[T]): Seq[T] = {
    lst match {
      case Nil => Nil
      case hd :: tl if hd == valueToFind => onto.reverse
      case hd :: tl => preceding1(tl, valueToFind, hd :: onto)
    }
  }
}
