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

package org.apache.daffodil.processors

import org.apache.daffodil.processors.dfa.DFADelimiter
import org.apache.daffodil.processors.parsers.DelimiterTextType
import scala.collection.mutable
import org.apache.daffodil.util.MStackOfInt

trait LocalDelimiters { this: DelimiterIterator =>
  val delimiterIndexStack: MStackOfInt

  override def reset() {
    currentIndex = delimiterIndexStack.top - 1
    indexLimit = this.delimiters.length
  }
}

trait RemoteDelimiters { this: DelimiterIterator =>
  val delimiterIndexStack: MStackOfInt

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

class LocalTypedDelimiterIterator(override val delimType: DelimiterTextType.Type, d: mutable.ArrayBuffer[DFADelimiter], override val delimiterIndexStack: MStackOfInt)
  extends DelimiterIterator(d)
  with LocalDelimiters
  with TypedDelimiter

class RemoteTypedDelimiterIterator(override val delimType: DelimiterTextType.Type, d: mutable.ArrayBuffer[DFADelimiter], override val delimiterIndexStack: MStackOfInt)
  extends DelimiterIterator(d)
  with RemoteDelimiters
  with TypedDelimiter

class RemoteTerminatingMarkupAndLocalTypedDelimiterIterator(localType: DelimiterTextType.Type, d: mutable.ArrayBuffer[DFADelimiter], delimiterIndexStack: MStackOfInt)
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
