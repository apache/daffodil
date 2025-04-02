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

import Maybe._

/**
 * Cursor pattern is a performance hack to avoid allocation of objects
 * purely due to an API constraint.
 *
 * Cursor[A] is an alternative to Iterator[T]. In Iterator[T] the next() method
 * (and peek() also) must construct an object of type T in order to return it.
 *
 * Cursor avoids this, by instead modifying an "accessor object" to contain the
 * next content (or peeked content).
 *
 * So you advance the cursor, access data using the accessor, and no allocation
 * is required by the API.
 *
 * The danger is that accessors are stateful, and must be per-thread.
 *
 * Contrast this with a callback-style API: the distinct fields of the accessor
 * would be passed as arguments to the callback method, so as to avoid
 * having to allocate an object of type T, but as there is no state object
 * being modified, a callback mechanism is always thread-safe.
 *
 * But the pattern of control it imposes is very troublesome. A cursor, like
 * an Iterator, is a "pull" style interface. It just doesn't pull new
 * objects, rather a pull operation causes a pre-existing state object to
 * be updated.
 *
 * So the accessor state objects must be thread-local, or part of some per-thread
 * state block.
 */

trait HasCpy[+ItemType] {
  def cpy(): ItemType
}

trait Accessor[ItemType] extends AnyRef with HasCpy[ItemType] {

  /**
   * Copy method that returns accessor's ItemType. (sometimes that's the same as the Accessor type).
   *
   * Not named copy() because of the automatically generated one from case-classes.
   *
   * Case classes are likely to be used a lot with accessors, and this doesn't have the same
   * type as the generated copy() methods.
   */
  def cpy(): ItemType

  /**
   * Used when you need to have 3 or more items in examination. You can instantiate an accessor,
   * and "advance" onto it by populating it using this method.
   *
   * Normally the advanceAccessor and inspectAccessor are sufficient, but sometimes
   * more are desirable.
   */
  def assignFrom(other: ItemType): Unit
}

trait Cursor[AccessorType <: Accessor[AccessorType]] {
  def advanceAccessor: AccessorType
  def inspectAccessor: AccessorType

  /**
   * Analogous to Iterator.next, except the accessor provides
   * access to the data, the returned boolean indicates whether
   * that happened successfully or there was no more data.
   */
  def advance: Boolean

  /**
   * Peek-like operation. Not called peek to avoid
   * confusion with iterators that have peek methods.
   *
   * The inspectAccessor provides access to the data, the returned
   * boolean indicates whether that happened successfully or there
   * was no more data.
   *
   * This has no side-effects on the infoset.
   */
  def inspect: Boolean

  /**
   * Convenient combinations of advance with
   * giving access to the appropriate accessor.
   */
  final def advanceMaybe: Maybe[AccessorType] = {
    if (advance) One(advanceAccessor)
    else Nope
  }

  final def inspectMaybe: Maybe[AccessorType] = {
    if (inspect) One(inspectAccessor)
    else Nope
  }

  /**
   * Cause this cursor to finish and cleanup anything that may be necessary,
   * regardless of if it is complete or not
   */
  def fini(): Unit
}

trait CursorImplMixin[AccessorType <: Accessor[AccessorType]] extends Cursor[AccessorType] {
  /*
   * We are a bit of a state machine based on what the last operation was.
   * At all times, we have a "current" element, which is what future calls to
   * advance/inspect provide information about.
   *
   * Our states our as follows:
   * priorOpKind == Advance - No work has been done for the current element. All work has been done on the prior element
   * priorOpKind == Inspect - The input corresponding to the current element has been consumed, and all exteranlly visible side effects have occured.
   */
  protected trait OpKind
  protected case object Advance extends OpKind
  protected case object Inspect extends OpKind
  protected case object Unsuccessful extends OpKind
  protected var priorOpKind: OpKind = Advance

  /**
   * Implement to fill in the accessor defined by the var`accessor`
   *
   * if advanceInput is false, then the relevent data from the underlying input stream has already been consumed,
   * and the prior value should be used.
   */
  protected def fill(advanceInput: Boolean): Boolean

  /**
   * Assign this var to whatever accessor you want filled by the next
   * advance or inspect operation.
   */
  protected final var accessor: AccessorType = null.asInstanceOf[AccessorType]

  private var isFilled = false

  final override def advance: Boolean = {
    accessor = advanceAccessor
    val res = priorOpKind match {
      case Advance => doAdvance(false, advanceInput = true)
      case Inspect => {
        // prior operation was inspect!
        priorOpKind = Advance
        advanceAccessor.assignFrom(inspectAccessor)
        isFilled = false
        true
      }
      case Unsuccessful => return false
    }
    // successful
    res
  }

  final override def inspect: Boolean = {
    accessor = inspectAccessor
    val res = priorOpKind match {
      case Advance => {
        priorOpKind = Inspect
        doAdvance(true, true)
      }
      case Inspect => true // inspect again does nothing.
      case Unsuccessful => return false
    }
    // successful
    Assert.invariant(isFilled == true || !res)
    res
  }

  /*
   * Logically speaking, a Cursor may have 2 "streams": an input stream and an output stream.
   *
   * When calling inspect/advance, we observe the current element of the output stream.
   * We advance the input stream the first time we observe an element, but do not advance the output
   * stream until the last time we observe an element (eg. until we observe an element through the advance() method).
   *
   * The advanceInput flag is needed so that calls to advance() can keep track if they are also the
   * first observations of the element, and therefore need to advance both the input stream and the output stream.
   */
  private def doAdvance(isFilledValue: Boolean, advanceInput: Boolean) = {
    val res = fill(advanceInput)
    if (!res) priorOpKind = Unsuccessful
    isFilled = res
    res
  }

}
