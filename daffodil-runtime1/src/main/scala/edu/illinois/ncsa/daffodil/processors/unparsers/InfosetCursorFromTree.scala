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

package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.DINode
import edu.illinois.ncsa.daffodil.processors.InfosetDocument
import edu.illinois.ncsa.daffodil.processors.InfosetElement
import edu.illinois.ncsa.daffodil.processors.InfosetItem
import edu.illinois.ncsa.daffodil.util.CursorImplMixin
import edu.illinois.ncsa.daffodil.util.MStackOf
import edu.illinois.ncsa.daffodil.util.MStackOfInt

/**
 * Iterates an infoset tree, handing out elements one by one in response to pull calls.
 */
private[unparsers] class InfosetCursorFromTree(item: InfosetItem)
    extends InfosetCursor
    with CursorImplMixin[InfosetAccessor] {

  private val nodeStack = new MStackOf[DINode]
  private val indexStack0b = MStackOfInt()

  private var visitKind: InfosetEventKind = StartKind

  val node = {
    val res = item match {
      case doc: InfosetDocument => doc.getRootElement()
      case el: InfosetElement => el
    }
    res.asInstanceOf[DINode]
  }

  nodeStack.push(node)
  indexStack0b.push(0)

  /**
   * In-order traversal of the nodes of an Infoset tree.
   *
   * State is kept in a nodeStack, and a corresponding indexStack0b. These
   * are always pushed and popped together - (two non-allocating stacks instead of one stack of 2-tuple)
   *
   * Note how the visitKind flag is used to keep track of order of arrival.
   */
  override def fill: Boolean = {
    if (nodeStack.isEmpty) return false
    val c = nodeStack.top
    visitKind match {
      case StartKind => {
        accessor.kind = visitKind
        accessor.node = c
        visitKind = EndKind
      }
      case EndKind => {
        val nextChildIndex0b = indexStack0b.pop
        indexStack0b.push(nextChildIndex0b + 1)
        if (nextChildIndex0b < c.numChildren) {
          // visit child
          val child = c.filledSlots(nextChildIndex0b)
          accessor.kind = StartKind
          accessor.node = child
          visitKind = EndKind
          nodeStack.push(child)
          indexStack0b.push(0)
        } else {
          // done with this complex node
          visitKind = EndKind
          accessor.kind = visitKind
          accessor.node = c
          nodeStack.pop
          indexStack0b.pop
        }
      }
    }
    true
  }

  override def fini: Unit = {}
}
