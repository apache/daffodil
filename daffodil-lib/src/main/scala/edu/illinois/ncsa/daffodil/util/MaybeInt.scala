package edu.illinois.ncsa.daffodil.util

/**
 * Uses a Long to store an Int so that we can still pass by Value, but we
 * can reserve a value to represent Nope.
 */
final case class MaybeInt private (__v: Long) extends AnyVal {
  @inline final def get: Int = if (isDefined) __v.toInt else noneGet
  @inline final def getOrElse(alternate: Int): Int = if (isDefined) get else alternate
  private def noneGet = throw new NoSuchElementException("Nope.get")
  @inline final def isDefined = __v != MaybeInt.undefValue
  @inline final def isEmpty = !isDefined
  override def toString = if (isEmpty) "Nope" else "MaybeInt(" + get + ")"

  // No map function or other monad features because we don't want usage
  // to EVER involve allocating closures/anonymous functions.
  //
  // The work-around: write an if-then-else like if (foo.isDefined) foo.get else MaybeUInt.Nope
  // verbose but known-to-be-fast
}

object MaybeInt {
  private val undefValue = Long.MaxValue

  @inline final def apply(v: Int) = new MaybeInt(v)

  val Nope = new MaybeInt(undefValue)
}

final case class MaybeChar private (__v: Int) extends AnyVal {
  @inline final def get: Char = if (isDefined) __v.toChar else noneGet
  @inline final def getOrElse(alternate: Char): Char = if (isDefined) get else alternate
  private def noneGet = throw new NoSuchElementException("Nope.get")
  @inline final def isDefined = __v != MaybeChar.undefValue
  @inline final def isEmpty = !isDefined
  override def toString = if (isEmpty) "Nope" else "MaybeChar(" + get + ")"

  // No map function or other monad features because we don't want usage
  // to EVER involve allocating closures/anonymous functions.
  //
  // The work-around: write an if-then-else like if (foo.isDefined) foo.get else MaybeUChar.Nope
  // verbose but known-to-be-fast
}

object MaybeChar {
  private val undefValue = -1

  @inline final def apply(v: Char) = new MaybeChar(v)

  val Nope = new MaybeChar(undefValue)
}

final case class MaybeBoolean private (__v: Int) extends AnyVal {
  @inline final def get: Boolean = if (isEmpty) noneGet else __v == 1
  @inline final def getOrElse(alternate: Boolean): Boolean = if (isDefined) get else alternate
  private def noneGet = throw new NoSuchElementException("Nope.get")
  @inline final def isDefined = __v != MaybeBoolean.undefValue
  @inline final def isEmpty = !isDefined
  override def toString = if (isEmpty) "Nope" else "MaybeBoolean(" + get + ")"

  // No map function or other monad features because we don't want usage
  // to EVER involve allocating closures/anonymous functions.
  //
  // The work-around: write an if-then-else like if (foo.isDefined) foo.get else MaybeUBoolean.Nope
  // verbose but known-to-be-fast
}

object MaybeBoolean {
  private val undefValue = -1

  @inline final def apply(v: Boolean) = new MaybeBoolean(if (v) 1 else 0)

  val Nope = new MaybeBoolean(undefValue)
}