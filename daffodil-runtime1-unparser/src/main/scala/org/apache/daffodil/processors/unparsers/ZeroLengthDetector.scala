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

package org.apache.daffodil.processors.unparsers

import org.apache.daffodil.infoset.DIElement
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.infoset.DISimple
import org.apache.daffodil.dpath.NodeInfo

/**
 * Rapidly determines if an Infoset Element will have zero length when unparsed.
 *
 * This is required for proper separator suppression in sequence unparsers.
 *
 * The specific concrete sub-class is selected based on a compile time analysis
 * of whether the element can have zero length at all, and if so, under
 * what conditions.
 */
sealed trait ZeroLengthDetector
  extends Serializable {
  def isZeroLength(diElement: DIElement): Boolean
}

/**
 * Applies to both simple and complex types that are nillable.
 */
sealed trait NillableZeroLengthMixin { self: ZeroLengthDetector =>
  protected def isNillableZeroLength(diElement: DIElement): Boolean = {
    Assert.invariant(diElement.erd.isNillable)
    diElement.isNilled
  }
}
/**
 * Use when the characteristics of the nil representation are such that
 * unparsing a nilled element will result in zero length.
 */
class NillableZeroLengthDetector
  extends ZeroLengthDetector
  with NillableZeroLengthMixin {
  override def isZeroLength(diElement: DIElement) = isNillableZeroLength(diElement)
}

sealed trait StringZeroLengthMixin { self: ZeroLengthDetector =>
  protected def isStringZeroLength(diElement: DIElement): Boolean = {
    val opt = diElement.erd.optPrimType
    Assert.invariant(opt.isDefined & (opt.get eq NodeInfo.String))
    val result =
      if (diElement.isNilled)
        false // must not be zero length nil. If it was zero length nil, we wouldn't be at this code.
      else {
        diElement match {
          case st: DISimple => {
            Assert.invariant(st.hasValue)
            val str = st.dataValueAsString
            val len = str.length()
            len == 0
          }
          case _ => Assert.invariantFailed("Not a simple type")
        }
      }
    result
  }
}

/**
 * Use when the element is of simple type string, not nillable.
 */
class StringZeroLengthDetector
  extends ZeroLengthDetector
  with StringZeroLengthMixin {
  override def isZeroLength(diElement: DIElement) = isStringZeroLength(diElement)

}

sealed trait HexBinaryZeroLengthMixin { self: ZeroLengthDetector =>
  protected def isHexBinaryZeroLength(diElement: DIElement): Boolean = {
    val opt = diElement.erd.optPrimType
    Assert.invariant(opt.isDefined & (opt.get eq NodeInfo.HexBinary))
    val result =
      if (diElement.isNilled)
        false // must not be zero length nil. If it was zero length nil, we wouldn't be at this code.
      else {
        diElement match {
          case st: DISimple => {
            Assert.invariant(st.hasValue)
            val arr = st.dataValue.asInstanceOf[Array[Byte]]
            val len = arr.length
            len == 0
          }
          case _ => Assert.invariantFailed("Not a simple type")
        }
      }
    result
  }
}

/**
 * Use when the element is of simple type hexBinary, not nillable.
 */
class HexBinaryZeroLengthDetector
  extends ZeroLengthDetector
  with HexBinaryZeroLengthMixin {
  override def isZeroLength(diElement: DIElement) = isHexBinaryZeroLength(diElement)
}

/**
 * Use when analysis shows the element can never be zero length.
 */
class NeverZeroLengthDetector
  extends ZeroLengthDetector {
  override def isZeroLength(diElement: DIElement) = false
}

/**
 * Use for nillable strings when they can be zero length two different ways.
 */
class NillableStringZeroLengthDetector
  extends ZeroLengthDetector
  with NillableZeroLengthMixin
  with StringZeroLengthMixin {
  override def isZeroLength(diElement: DIElement) =
    isStringZeroLength(diElement) ||
      isNillableZeroLength(diElement)
}

/**
 * Use for nillable hexBinary when they can be zero length two different ways.
 */
class NillableHexBinaryZeroLengthDetector
  extends ZeroLengthDetector
  with NillableZeroLengthMixin
  with HexBinaryZeroLengthMixin {
  override def isZeroLength(diElement: DIElement) =
    isHexBinaryZeroLength(diElement) ||
      isNillableZeroLength(diElement)
}
