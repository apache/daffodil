/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.util

import edu.illinois.ncsa.daffodil.exceptions.Assert
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
  def fini: Unit
}

trait CursorImplMixin[AccessorType <: Accessor[AccessorType]] { self: Cursor[AccessorType] =>

  private trait OpKind
  private case object Advance extends OpKind
  private case object Inspect extends OpKind
  private case object Unsuccessful extends OpKind
  private var priorOpKind: OpKind = Advance

  /**
   * Implement to fill in the accessor defined by the var`accessor`
   */
  protected def fill: Boolean

  /**
   * Assign this var to whatever accessor you want filled by the next
   * advance or inspect operation.
   */
  protected final var accessor: AccessorType = null.asInstanceOf[AccessorType]

  private var isFilled = false

  final override def advance: Boolean = {
    accessor = advanceAccessor
    val res = priorOpKind match {
      case Advance => doAdvance(false)
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
    Assert.invariant(isFilled == false)
    res
  }

  final override def inspect: Boolean = {
    accessor = inspectAccessor
    val res = priorOpKind match {
      case Advance => {
        priorOpKind = Inspect
        doAdvance(true)
      }
      case Inspect => true // inspect again does nothing.
      case Unsuccessful => return false
    }
    // successful
    Assert.invariant(isFilled == true || !res)
    res
  }

  private def doAdvance(isFilledValue: Boolean) = {
    Assert.invariant(isFilled == false)
    val res = fill
    if (!res) priorOpKind = Unsuccessful
    isFilled = if (res) isFilledValue else false
    res
  }

}
