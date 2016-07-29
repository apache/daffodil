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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.processors.dfa.DFADelimiter
import edu.illinois.ncsa.daffodil.processors.parsers.DelimiterTextType
import edu.illinois.ncsa.daffodil.util.MStack
import scala.collection.mutable


trait LocalDelimiters { this: DelimiterIterator =>
  val delimiterIndexStack: MStack.OfInt

  override def reset() {
    currentIndex = delimiterIndexStack.top - 1
    indexLimit = this.delimiters.length
  }
}

trait RemoteDelimiters { this: DelimiterIterator =>
  val delimiterIndexStack: MStack.OfInt

  override def reset() {
    currentIndex = -1
    indexLimit = delimiterIndexStack.top
  }
}

trait RemoteAndLocalDelimiters { this: DelimiterIterator =>
  override def reset() {
    currentIndex = -1
    indexLimit = this.delimiters.length
  }
}


trait TypedDelimiter { this: DelimiterIterator =>
  protected def delimType: DelimiterTextType.Type
 
  override def isOfInterest(delim: DFADelimiter): Boolean = {
    delim.delimType == delimType
  }
}

trait TerminatingMarkup { this: DelimiterIterator =>
  override def isOfInterest(delim: DFADelimiter) = {
    delim.delimType == DelimiterTextType.Terminator || delim.delimType == DelimiterTextType.Separator
  }
}

trait Separators { this: DelimiterIterator =>
  @inline
  override def isOfInterest(delim: DFADelimiter) = {
    delim.delimType == DelimiterTextType.Separator
  }
}

trait Initiators { this: DelimiterIterator =>
  @inline
  override def isOfInterest(delim: DFADelimiter) = {
    delim.delimType == DelimiterTextType.Initiator
  }
}

trait Terminators { this: DelimiterIterator =>
  @inline
  override def isOfInterest(delim: DFADelimiter) = {
    delim.delimType == DelimiterTextType.Terminator
  }
}


abstract class DelimiterIterator(val delimiters: mutable.ArrayBuffer[DFADelimiter]) {

  protected def isOfInterest(delim: DFADelimiter): Boolean
  def reset(): Unit

  var currentIndex = -1
  var indexLimit = -1

  // TODO: I'm a little concernece that this doesn't follow standard iterator
  // practicies. For exmaple, if you called di.next() twice in a row without
  // calling hasNext, the second next() call would give you the same delimiter.
  // So for this to work, you really do need to call hasNext before ever
  // calling next. Although one should follow that practice with iterators, it
  // isn't usually required.
  def hasNext(): Boolean = {
    currentIndex += 1
    while (currentIndex >= 0 && currentIndex < indexLimit) {
      if (isOfInterest(delimiters(currentIndex))) {
        return true      
      }
      currentIndex += 1
    }
    return false
  }

  def next(): DFADelimiter = {
    delimiters(currentIndex)
  }
}

class AllTerminatingMarkupDelimiterIterator(d: mutable.ArrayBuffer[DFADelimiter])
  extends DelimiterIterator(d)
  with RemoteAndLocalDelimiters
  with TerminatingMarkup

class LocalTypedDelimiterIterator(override val delimType: DelimiterTextType.Type, d: mutable.ArrayBuffer[DFADelimiter], override val delimiterIndexStack: MStack.OfInt)
  extends DelimiterIterator(d)
  with LocalDelimiters
  with TypedDelimiter

class RemoteTypedDelimiterIterator(override val delimType: DelimiterTextType.Type, d: mutable.ArrayBuffer[DFADelimiter], override val delimiterIndexStack: MStack.OfInt)
  extends DelimiterIterator(d)
  with RemoteDelimiters
  with TypedDelimiter

class RemoteTerminatingMarkupAndLocalTypedDelimiterIterator(localType: DelimiterTextType.Type, d: mutable.ArrayBuffer[DFADelimiter], delimiterIndexStack: MStack.OfInt)
  extends DelimiterIterator(d)
  with RemoteAndLocalDelimiters {
 
  override def isOfInterest(delim: DFADelimiter): Boolean = {
    if (currentIndex < delimiterIndexStack.top) {
      delim.delimType == DelimiterTextType.Terminator || delim.delimType == DelimiterTextType.Separator
    } else {
      delim.delimType == localType
    }
  }
}

class AllDelimiterIterator(d: mutable.ArrayBuffer[DFADelimiter])
  extends DelimiterIterator(d)
  with RemoteAndLocalDelimiters {

  override def isOfInterest(delim: DFADelimiter) = true
}
