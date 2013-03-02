package edu.illinois.ncsa.daffodil.util
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG

/**
 * We need a list-like/stream-like thing, but that is
 * lazy in both the head and tail. The built in streams in
 * Scala are only tail-lazy.
 *
 * We call it ZList as in 'Z' as in being lazy, as in sleeping as in "ZZzzzzz..."
 */

object ZList {
  def uid = {
    val res = uid_
    uid_ += 1
    res
  }
  private var uid_ = 0L
}

abstract class ZList {
  def head: Any
  def tail: ZList
  protected var headVal = false
  protected var tailVal = false
  protected val uid = ZList.uid

  // print out no matter what goes wrong when evaluating.
  override def toString = {
    "Z@" + uid + "(" +
      (if (headVal)
        OOLAG.keepGoing("noValue") { head.toString }
      else "...") +
      ", " +
      (if (tailVal)
        OOLAG.keepGoing("noTail") { tail.toString }
      else "...") +
      ")"

  }
}
/**
 * lazy (sleeping) on both arguments hence double Z.
 */
class ZZ private (h: => Any, t: => ZList) extends ZList {
  lazy val head = { headVal = true; h }
  lazy val tail = { tailVal = true; t }
}

object ZZ {
  def apply(head: => Any, tail: => ZList) = new ZZ(head, tail)
}

object ZEnd extends ZList {
  lazy val head = Assert.usageError("head of ZEnd")
  lazy val tail = Assert.usageError("tail of ZEnd")
  override def toString = "ZEnd"
}