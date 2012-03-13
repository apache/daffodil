package daffodil.util



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

  def preceding[T](lst: Seq[T], valueToFind: Any) = preceding1(lst, valueToFind, Nil)

  private def preceding1[T](lst: Seq[T], valueToFind: Any, onto: List[T]): Seq[T] = {
    lst match {
      case Nil => Nil
      case hd :: tl => {
        if (tl == valueToFind) hd :: onto
        else preceding1(tl, valueToFind, hd :: onto)
      }
    }
  }
}
