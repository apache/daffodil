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

package org.apache.daffodil.runtime1.processors

import scala.collection.mutable

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.runtime1.processors.dfa.DFADelimiter
import org.apache.daffodil.runtime1.processors.parsers.DelimiterTextType

trait DelimitersRangeLocal { this: DelimiterIterator =>
  override def reset(): Unit = {
    currentIndex = this.firstLocalIndex - 1
    indexLimit = this.delimiters.length
  }
}

trait DelmitersRangeRemote { this: DelimiterIterator =>
  override def reset(): Unit = {
    currentIndex = -1
    indexLimit = this.firstLocalIndex
  }
}

trait DelimitersRangeAll { this: DelimiterIterator =>
  override def reset(): Unit = {
    currentIndex = -1
    indexLimit = this.delimiters.length
  }
}

/**
 * It's important to note that this does not follow standard iterator
 * practices.
 *
 * First, reset() must always be called before iteration beings. This is
 * because this function initializes the indices so that only the correct
 * proper range of delimiters are scanned for. Without calling this, the
 * iterator will return nothing.
 *
 * Second, hasNext() is what moves the iterator to the next delimiter, not
 * next(). next() simply returns the current index in the iterator. So calling
 * next() twice in a row will return the same thing. Calling hasNext before
 * calling next is standard iterator practice, though, and how we always use
 * it, but it is actually required.
 *
 * So this DelmiterIterators should always be used like so:
 *
 *   val iter = new DelimiterIterator()
 *   iter.reset()
 *   while (iter.hasNext()) {
 *     val delim = iter.next()
 *     ...
 *   }
 *
 */
abstract class DelimiterIterator(val delimiters: mutable.ArrayBuffer[DFADelimiter]) {

  protected def isOfInterest(delim: DFADelimiter): Boolean
  def firstLocalIndex: Int
  def reset(): Unit

  var currentIndex = -1
  var indexLimit = -1

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

  def isRemote(): Boolean = currentIndex < firstLocalIndex
}

class AllTerminatingMarkupDelimiterIterator(
  d: mutable.ArrayBuffer[DFADelimiter],
  override val firstLocalIndex: Int
) extends DelimiterIterator(d)
  with DelimitersRangeAll {

  override def isOfInterest(delim: DFADelimiter): Boolean = {
    // include only term/sep, ignore all ES
    if (delim.isES) false
    else
      delim.delimType == DelimiterTextType.Terminator || delim.delimType == DelimiterTextType.Separator
  }
}

class LocalTypedDelimiterIterator(
  delimType: DelimiterTextType.Type,
  d: mutable.ArrayBuffer[DFADelimiter],
  override val firstLocalIndex: Int
) extends DelimiterIterator(d)
  with DelimitersRangeLocal {

  override def isOfInterest(delim: DFADelimiter): Boolean = {
    // include ES's, the DelmiterRangeLocal will ensure we ignore remote delims
    delim.delimType == delimType
  }
}

class RemoteTerminatingMarkupAndLocalTypedDelimiterIterator(
  localType: DelimiterTextType.Type,
  d: mutable.ArrayBuffer[DFADelimiter],
  override val firstLocalIndex: Int
) extends DelimiterIterator(d)
  with DelimitersRangeAll {

  override def isOfInterest(delim: DFADelimiter): Boolean = {
    // all remote term/sep excluding ES, or all local typed including ES
    if (isRemote()) {
      if (delim.isES) false
      else
        delim.delimType == DelimiterTextType.Terminator || delim.delimType == DelimiterTextType.Separator
    } else {
      delim.delimType == localType
    }
  }
}

class AllDelimiterIterator(d: mutable.ArrayBuffer[DFADelimiter])
  extends DelimiterIterator(d)
  with DelimitersRangeAll {

  override val firstLocalIndex: Int = 0
  override def isRemote(): Boolean =
    Assert.usageError("Not be used, no concept of local/remote here")

  override def isOfInterest(delim: DFADelimiter) = {
    // all delimiters except for ES
    !delim.isES
  }
}
