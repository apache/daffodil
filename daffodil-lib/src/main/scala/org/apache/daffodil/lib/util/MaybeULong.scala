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

import org.apache.daffodil.lib.exceptions.Assert

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
  // @inline final def getOrElse(alternate: Long): Long = if (isDefined) get else alternate
  // @inline final def getULongOrElse(alternate: ULong): ULong = ULong(getOrElse(alternate.toLong))
  private def noneGet = throw new NoSuchElementException("Nope.get")
  @inline final def isDefined = __rep != MaybeULong.undefValue
  @inline final def isEmpty = !isDefined
  @inline final def toOption: Option[Long] = if (isDefined) Some(get) else None
  override def toString = if (isEmpty) "Nope" else "One(" + get + ")"

  // No map function or other monad features because we don't want usage
  // to EVER involve allocating closures/anonymous functions.
  //
  // The work-around: write an if-then-else like if (foo.isDefined) foo.get else MaybeULong.Nope
  // verbose but known-to-be-fast

  @inline def toMaybeJULong =
    if (isEmpty) MaybeJULong.Nope else new MaybeJULong(MaybeULong(__rep))
}

object MaybeULong {
  private val undefValue = -1L

  @inline final def apply(v: Long) = {
    Assert.usage(v >= 0)
    new MaybeULong(v)
  }

  val Nope = new MaybeULong(undefValue)
}

/**
 * Maybe[ULong] still isn't an AnyRef, We need an AnyRef class
 * that can be used to pass/return/store One(ULong) or Nope.
 *
 * Like a MaybeULong, but an AnyRef, so can be stored in
 * collections.
 *
 * This reduces boxing. You get one object with an unboxed MaybeULong
 * stored within it.
 */
final class MaybeJULong(mi: MaybeULong) extends Serializable {
  @inline final def get: Long = mi.get
  @inline final def getULong = mi.getULong
  // @inline final def getOrElse(alternate: Long): Long = mi.getOrElse(alternate)
  // @inline final def getULongOrElse(alternate: ULong): ULong = mi.getULongOrElse(alternate)
  @inline final def isDefined = mi.isDefined
  @inline final def isEmpty = !isDefined
  @inline final def toOption: Option[Long] = if (isDefined) Some(get) else None
  override def toString = mi.toString

  @inline def toMaybeULong = mi
}

object MaybeJULong {

  @inline def apply(v: Long) = new MaybeJULong(MaybeULong(v))

  val Nope = new MaybeJULong(MaybeULong.Nope)
}
