/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.lib.util

/*
 * Performance Note:
 *
 * These exist for perforance reasons, to avoid boxing objects just to
 * get Maybe[T]/Option[T] behavior.
 *
 * Everything (pretty much) inlines on these classes. So their overhead is
 * presumably zero.
 *
 * Maybe[T] already is just a performance optimization on Option[T]
 * because it is a value class (i.e., AnyVal), not an object like Option[T].
 *
 * These specialized versions like MaybeInt
 * aren't @specialized(Int) versions of Maybe[Int], because value classes
 * cannot be derived from a base. (At least in Scala 2.12).
 *
 * Using Maybe[Int] with specialization would require that the Maybe[T]
 * class took a parameter which is the specialized reserved undef value,
 * and the "underlying type", which for MaybeInt, is a Long. You can't
 * derive one value class (AnyVal) from another because then you'd have the
 * possibility of polymorphism (overloaded methods) which AnyVal non-objects
 * cannot support.
 *
 * So it's really not a good idea to try to "clean up" this stuff using
 * Scala 2.12 specialization of generic types. Maybe in the future
 * Scala will have a kind of specialization and AnyVal support that is up
 * to the job.
 */

/**
 * Uses a Long to store an Int so that we can still pass by Value, but we
 * can reserve a value to represent Nope.
 */
final case class MaybeInt private (__v: Long) extends AnyVal {
  @inline final def get: Int = if (isDefined) __v.toInt else noneGet
  // @inline final def getOrElse(alternate: Int): Int = if (isDefined) get else alternate
  private def noneGet = throw new NoSuchElementException("Nope.get")
  @inline final def isDefined = __v != MaybeInt.undefValue
  @inline final def isEmpty = !isDefined
  @inline final def toOption: Option[Int] = if (isDefined) Some(get) else None
  override def toString = if (isEmpty) "Nope" else "MaybeInt(" + get + ")"

  // No map function or other monad features because we don't want usage
  // to EVER involve allocating closures/anonymous functions.
  //
  // The work-around: write an if-then-else like if (foo.isDefined) foo.get else MaybeUInt.Nope
  // verbose but known-to-be-fast

  @inline final def toMaybeJInt = if (isEmpty) MaybeJInt.Nope else new MaybeJInt(MaybeInt(__v))
}

object MaybeInt {

  /**
   * Use to do Maybe-like things using a reserved value vs. a normal value.
   *
   * Sometimes you have to have an AnyRef, so you need to use a JLong, not
   * a MaybeInt.
   *
   * But you want "conceptually" the same thing as a MaybeInt.
   *
   * A Maybe[JInt] won't work, because that's not an AnyRef either.
   */
  type Type = Long

  val undefValue: Type = Long.MaxValue

  @inline def isDefined(v: Type): Boolean = v == undefValue
  @inline def isEmpty(v: Type): Boolean = !isDefined(v)

  @inline def apply(v: Int) = new MaybeInt(v)

  val Nope = new MaybeInt(undefValue)

}

/**
 * Maybe[JInt] still isn't an AnyRef, We need an AnyRef class
 * that can be used to pass/return/store One(Int) or Nope.
 *
 * Like a Maybe[Int], but an AnyRef, so can be stored in
 * collections.
 *
 * This reduces boxing. You get one object with an unboxed MaybeInt
 * stored within it.
 */
final class MaybeJInt(mi: MaybeInt) {
  @inline final def get: Int = mi.get
  // @inline final def getOrElse(alternate: Int): Int = mi.getOrElse(alternate)
  @inline final def isDefined = mi.isDefined
  @inline final def isEmpty = !isDefined
  @inline final def toOption: Option[Int] = if (isDefined) Some(get) else None
  override def toString = mi.toString
}

object MaybeJInt {

  @inline def apply(v: Int) = new MaybeJInt(MaybeInt(v))

  val Nope = new MaybeJInt(MaybeInt.Nope)
}

final case class MaybeChar private (__v: Int) extends AnyVal {
  @inline final def get: Char = if (isDefined) __v.toChar else noneGet
  // @inline final def getOrElse(alternate: Char): Char = if (isDefined) get else alternate
  private def noneGet = throw new NoSuchElementException("Nope.get")
  @inline final def isDefined = __v != MaybeChar.undefValue
  @inline final def isEmpty = !isDefined
  @inline final def toOption: Option[Char] = if (isDefined) Some(get) else None
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

final class MaybeBoolean private (val __v: Int) extends AnyVal {
  @inline final def get: Boolean = if (isEmpty) noneGet else __v == 1
  // @inline final def getOrElse(alternate: Boolean): Boolean = if (isDefined) get else alternate
  private def noneGet = throw new NoSuchElementException("Nope.get")
  @inline final def isDefined = __v != MaybeBoolean.undefValue
  @inline final def isEmpty = !isDefined
  @inline final def toOption: Option[Boolean] = if (isDefined) Some(get) else None
  override def toString = if (isEmpty) "Nope" else "MaybeBoolean(" + get + ")"

  // No map function or other monad features because we don't want usage
  // to EVER involve allocating closures/anonymous functions.
  //
  // The work-around: write an if-then-else like if (foo.isDefined) foo.get else MaybeUBoolean.Nope
  // verbose but known-to-be-fast
}

object MaybeBoolean {
  private val undefValue = -1

  @inline final def apply(v: Boolean) = if (v) MaybeBoolean.True else MaybeBoolean.False

  val Nope = new MaybeBoolean(undefValue)
  val True = new MaybeBoolean(1)
  val False = new MaybeBoolean(0)

}
