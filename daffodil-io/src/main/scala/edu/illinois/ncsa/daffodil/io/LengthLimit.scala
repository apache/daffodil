package edu.illinois.ncsa.daffodil.io
//
//import edu.illinois.ncsa.daffodil.exceptions.Assert
//import passera.unsigned.ULong
//
///**
// * if isDefined then a Long with non-negative value.
// *
// * Use to replace untyped use of Long where -1 means undefined.
// * So forgetting to test for the -1 case is not possible. You must
// * call the get method to get the unvarnished value.
// */
//final class LengthLimit private (val __v: Long) extends AnyVal {
//  @inline def get: Long = if (isDefined) __v else noneGet
//  @inline def getULong = ULong(get)
//  @inline def getOrElse(alternate: Long): Long = if (isDefined) get else alternate
//  private def noneGet = throw new NoSuchElementException("Undefined get")
//  @inline def isDefined = __v != LengthLimit.undefValue
//  @inline def isEmpty = !isDefined
//  override def toString = if (isEmpty) "LengthLimit(undefined)" else "LengthLimit(" + get + ")"
//
//  @deprecated("2015-09-23", "For performance reasons replace use of map with a macro or use if-then-else explicitly")
//  @inline def map(f: Long => Long) = if (isDefined) LengthLimit(f(__v)) else LengthLimit.Undefined
//}
//
//object LengthLimit {
//  private val undefValue = -1L
//  @inline def apply(v: Long) = {
//    Assert.usage(v >= 0)
//    new LengthLimit(v)
//  }
//  val Undefined = new LengthLimit(undefValue)
//}