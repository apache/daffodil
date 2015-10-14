package edu.illinois.ncsa.daffodil.util

import edu.illinois.ncsa.daffodil.exceptions.Assert
import passera.unsigned.ULong

/**
 * if isDefined then an unsigned long
 *
 * Use to replace untyped use of Long where -1 means undefined.
 * So forgetting to test for the -1 case is not possible. You must
 * call the get method to get the unvarnished ULong value, and then ask for the
 * toLong of that if you want a plain Long.
 */
final class MaybeULong private (val __rep: Long) extends AnyVal {
  @inline final def get: Long = if (isDefined) __rep else noneGet
  @inline final def getULong: ULong = ULong(__rep)
  @inline final def getOrElse(alternate: Long): Long = if (isDefined) get else alternate
  @inline final def getULongOrElse(alternate: ULong): ULong = ULong(getOrElse(alternate.toLong))
  private def noneGet = throw new NoSuchElementException("Nope.get")
  @inline final def isDefined = __rep != MaybeULong.undefValue
  @inline final def isEmpty = !isDefined
  override def toString = if (isEmpty) "Nope" else "One(" + get + ")"

  // No map function or other monad features because we don't want usage
  // to EVER involve allocating closures/anonymous functions.
  //
  // The work-around: write an if-then-else like if (foo.isDefined) foo.get else MaybeULong.Nope
  // verbose but known-to-be-fast
}

object MaybeULong {
  private val undefValue = -1L
  @inline final def apply(v: Long) = {
    Assert.usage(v >= 0)
    new MaybeULong(v)
  }
  val Nope = new MaybeULong(undefValue)
}

