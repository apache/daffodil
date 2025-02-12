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

import java.util
import scala.collection.mutable.ArrayBuffer

/**
 * TODO: scala 2.12 phase out
 * Compatibility ArrayBuffer class for 2.12 and 2.13 since reduceToSize
 * has been removed in 2.13. This allows us to maintain the same
 * functionality as 2.12 while upgraded to 2.13
 */
class ArrayBuffer1[T](initialSize: Int = 16) extends ArrayBuffer[T] {
  // Preallocate space to avoid frequent resizing
  this.sizeHint(initialSize)

  def reduceToSize1(sz: Int): Unit = {
    if (sz >= 0 && sz <= size0) {
      // to ensure things are not left un-garbage collected
      util.Arrays.fill(array, sz, size0, null)
      size0 = sz
    } else
      throw new IndexOutOfBoundsException(
        "Invalid size: Must be between 0 and the current size of the buffer."
      )
  }
}

object ArrayBuffer1 extends ArrayBuffer {
  def apply[T](ab: ArrayBuffer[T]): ArrayBuffer1[T] = {
    val s = ab.length
    val arr = new ArrayBuffer1[T](s)
    arr ++= ab
    arr
  }
}
