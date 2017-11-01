/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.infoset

import org.apache.daffodil.Implicits._;
import org.apache.daffodil.xml.NS
import org.apache.daffodil.util.MaybeBoolean
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.processors.ElementRuntimeData

object INoWarn2 { ImplicitsSuppressUnusedImportWarning() }

trait InfosetCommon {
  def visit(handler: InfosetOutputter, removeHidden: Boolean = true): Unit
}

trait InfosetArray extends InfosetCommon {
  def append(ie: InfosetElement): Unit
  def getOccurrence(occursIndex: Long): InfosetElement
  def length: Long

}

trait InfosetElement extends InfosetItem {

  def parent: InfosetComplexElement
  def setParent(p: InfosetComplexElement): Unit

  def array: Maybe[InfosetArray]
  def setArray(a: InfosetArray): Unit

  def isNilled: Boolean
  def setNilled(): Unit

  def isEmpty: Boolean

  def valid: MaybeBoolean
  def setValid(validity: Boolean): Unit

  /**
   * Retrieve the schema component that gave rise to this infoset
   * item.
   */
  def runtimeData: ElementRuntimeData
  def namespace: NS
  def name: String
  def isHidden: Boolean
}

trait InfosetComplexElement extends InfosetElement {

  def getChild(erd: ElementRuntimeData): InfosetElement
  def getChildArray(erd: ElementRuntimeData): InfosetArray

  /**
   * Determines slotInParent from the ERD of the infoset element arg.
   * Hooks up the parent pointer of the new child to reference this.
   *
   * When slot contains an array, this appends to the end of the array.
   */
  def addChild(e: InfosetElement): Unit

}

trait InfosetSimpleElement extends InfosetElement {

  def dataValue: Any

  /**
   * Caches the string so we're not allocating strings just to do facet checks
   */
  def dataValueAsString: String
  def setDataValue(s: AnyRef): Unit
  def isDefaulted: Boolean
}

trait InfosetDocument extends InfosetItem {
  def getRootElement(): InfosetElement
  def setRootElement(root: InfosetElement): Unit
}

trait InfosetItem extends InfosetCommon {
  /**
   * The totalElementCount is the total count of how many elements this InfosetItem contains.
   *
   * (Used to call this 'size', but size is often a length-like thing, so changed name
   * to be more distinctive)
   */
  def totalElementCount: Long
}
