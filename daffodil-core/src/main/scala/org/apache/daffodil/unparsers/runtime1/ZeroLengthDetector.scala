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

package org.apache.daffodil.unparsers.runtime1

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.infoset.DIElement
import org.apache.daffodil.runtime1.infoset.DISimple

/**
 * Rapidly determines if an Infoset Element will have non-zero length when unparsed.
 *
 * This enables an important optimization for separator suppression.
 *
 * If by looking at the infoset value/nilled status we can tell whether it would
 * be non-zero length immediately, then we don't need to suspend whether to unparse
 * the separator or not, we just unparse it directly.
 *
 * Conversely, if we don't know if the value will unparse to zero-length or not,
 * we must use a suspendable unparser for the separator, and that has a lot of overhead.
 * But we avoid this overhead in most cases.
 *
 * The specific concrete sub-class is selected based on a compile time analysis
 * of whether the element can have zero length at all, and if so, under
 * what conditions.
 */
sealed trait ZeroLengthDetector extends Serializable {

  /**
   * Determines if the representation is known for certain to be non-zero length.
   *
   * Used to avoid complexity when unparsing and trailing separator suppression is
   * a possibility.
   */
  def isKnownNonZeroLength(diElement: DIElement): Boolean

  /**
   * Determines if the representation is known for certain to be zero length.
   *
   * Used to optimize for dfdl:separatorSuppressionPolicy="anyEmpty".
   */
  def isKnownZeroLength(diElement: DIElement): Boolean

  def isKnownNonZeroLengthModelGroup: Boolean = Assert.usageError("Not for use on elements")
  def isKnownZeroLengthModelGroup: Boolean = Assert.usageError("Not for use on elements")
}

/**
 * Applies to both simple and complex types that are nillable when the
 * nil representation is zero-length, and the non-nil representation cannot
 * be zero length.
 */
sealed trait NillableZeroLengthMixin { self: ZeroLengthDetector =>
  protected def isNillableKnownNonZeroLength(diElement: DIElement): Boolean = {
    Assert.invariant(diElement.erd.isNillable)
    !diElement.isNilled
  }
  protected def isNillableKnownZeroLength(diElement: DIElement): Boolean = {
    Assert.invariant(diElement.erd.isNillable)
    diElement.isNilled
  }
}

/**
 * Use when the characteristics of the nil representation are such that
 * unparsing a nilled element will result in non-zero length.
 *
 * This is used for complex types, and simple types that cannot have a zero-length
 * representation other than the nil representation, meaning not strings, and
 * not hexBinary.
 */
class NillableZeroLengthDetector extends ZeroLengthDetector with NillableZeroLengthMixin {
  override def isKnownNonZeroLength(diElement: DIElement) = isNillableKnownNonZeroLength(
    diElement
  )
  override def isKnownZeroLength(diElement: DIElement) = isNillableKnownZeroLength(diElement)
}

sealed trait StringZeroLengthMixin { self: ZeroLengthDetector =>

  protected def isStringKnownNonZeroLength(diElement: DIElement): Boolean = {
    val erd = diElement.erd
    val opt = erd.optPrimType
    Assert.invariant(opt.isDefined & (opt.get eq NodeInfo.String))
    val result =
      if (diElement.isNilled) {
        // Must not be zero length nil. If it was zero length nil, we wouldn't be at this code.
        // It would be good to check this with an assertion here, but the RuntimeData objects
        // don't (as of writing this code) tell us the length of the nil representation being
        // used for unparsing.
        true
      } else {
        diElement match {
          case st: DISimple => {
            // Assert.invariant(st.hasValue) - Wrong. This might not have value if defaultable.
            val str = st.dataValueAsString
            val len = str.length()
            len > 0
          }
          case _ => Assert.invariantFailed("Not a simple type")
        }
      }
    result
  }

  protected def isStringKnownZeroLength(diElement: DIElement): Boolean = {
    val erd = diElement.erd
    val opt = erd.optPrimType
    Assert.invariant(opt.isDefined & (opt.get eq NodeInfo.String))
    val result =
      if (diElement.isNilled) {
        // Must not be zero length nil. If it was zero length nil, we wouldn't be at this code.
        // It would be good to check this with an assertion here, but the RuntimeData objects
        // don't (as of writing this code) tell us the length of the nil representation being
        // used for unparsing.
        false
      } else {
        diElement match {
          case st: DISimple => {
            // Assert.invariant(st.hasValue) - Wrong. This might not have value if defaultable.
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
class StringZeroLengthDetector extends ZeroLengthDetector with StringZeroLengthMixin {
  override def isKnownNonZeroLength(diElement: DIElement) = isStringKnownNonZeroLength(
    diElement
  )
  override def isKnownZeroLength(diElement: DIElement) = isStringKnownZeroLength(diElement)

}

sealed trait HexBinaryZeroLengthMixin { self: ZeroLengthDetector =>

  protected def isHexBinaryKnownNonZeroLength(diElement: DIElement): Boolean = {
    val opt = diElement.erd.optPrimType
    Assert.invariant(opt.isDefined & (opt.get eq NodeInfo.HexBinary))
    val result =
      if (diElement.isNilled) {
        // if the element isNilled, we should already have predicted ZL/not
        // for the nilled case. So here we know if it is nilled then
        // it is definitely NOT zero length representation for nil.
        // There is no runtime data structure to tell us whether the nil
        // representation when unparsed, is zero-length or not. So we can't
        // check this invariant here.
        true
      } else {
        diElement match {
          case st: DISimple => {
            val arr = st.dataValue.getByteArray
            val len = arr.length
            len > 0
          }
          case _ => Assert.invariantFailed("Not a simple type")
        }
      }
    result
  }

  protected def isHexBinaryKnownZeroLength(diElement: DIElement): Boolean = {
    val opt = diElement.erd.optPrimType
    Assert.invariant(opt.isDefined & (opt.get eq NodeInfo.HexBinary))
    val result =
      if (diElement.isNilled)
        false // must not be zero length nil. If it was zero length nil, we wouldn't be at this code.
      else {
        diElement match {
          case st: DISimple => {
            val arr = st.dataValue.getByteArray
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
class HexBinaryZeroLengthDetector extends ZeroLengthDetector with HexBinaryZeroLengthMixin {
  override def isKnownNonZeroLength(diElement: DIElement) = isHexBinaryKnownNonZeroLength(
    diElement
  )
  override def isKnownZeroLength(diElement: DIElement) = isHexBinaryKnownZeroLength(diElement)
}

/**
 * Use when analysis shows the element can never be zero length.
 */
object NeverZeroLengthDetector extends ZeroLengthDetector {
  override def isKnownNonZeroLength(diElement: DIElement) = true
  override def isKnownZeroLength(diElement: DIElement) = false
  override def isKnownNonZeroLengthModelGroup = true
  override def isKnownZeroLengthModelGroup = false
}

object PossiblyZeroLengthModelGroupDetector extends ZeroLengthDetector {
  override def isKnownNonZeroLength(diElement: DIElement) =
    Assert.usageError("Not for use on Model Groups")
  override def isKnownZeroLength(diElement: DIElement) =
    Assert.usageError("Not for use on Model Groups")
  override def isKnownNonZeroLengthModelGroup = false
  override def isKnownZeroLengthModelGroup = false
}

object PossiblyZeroArrayOccurrencesDetector extends ZeroLengthDetector {
  override def isKnownNonZeroLength(diElement: DIElement) = false
  override def isKnownZeroLength(diElement: DIElement) = false
  override def isKnownNonZeroLengthModelGroup =
    Assert.usageError("Not for use on recurring elements")
  override def isKnownZeroLengthModelGroup =
    Assert.usageError("Not for use on recurring elements")
}

object NotRepresentedZeroLengthDetector extends ZeroLengthDetector {
  override def isKnownNonZeroLength(diElement: DIElement) =
    Assert.usageError("Not for use on computed elements")
  override def isKnownZeroLength(diElement: DIElement) =
    Assert.usageError("Not for use on computed elements")
  override def isKnownNonZeroLengthModelGroup =
    Assert.usageError("Not for use on computed elements")
  override def isKnownZeroLengthModelGroup =
    Assert.usageError("Not for use on computed elements")
}

/**
 * Use for nillable strings when they can be zero length two different ways.
 */
class NillableStringZeroLengthDetector
  extends ZeroLengthDetector
  with NillableZeroLengthMixin
  with StringZeroLengthMixin {

  override def isKnownNonZeroLength(diElement: DIElement) =
    if (diElement.isNilled)
      isNillableKnownNonZeroLength(diElement)
    else isStringKnownNonZeroLength(diElement)

  override def isKnownZeroLength(diElement: DIElement) =
    if (diElement.isNilled)
      isNillableKnownZeroLength(diElement)
    else
      isStringKnownNonZeroLength(diElement)
}

/**
 * Use for nillable hexBinary when they can be zero length two different ways.
 */
class NillableHexBinaryZeroLengthDetector
  extends ZeroLengthDetector
  with NillableZeroLengthMixin
  with HexBinaryZeroLengthMixin {

  override def isKnownNonZeroLength(diElement: DIElement) =
    if (diElement.isNilled)
      isNillableKnownNonZeroLength(diElement)
    else isHexBinaryKnownNonZeroLength(diElement)

  override def isKnownZeroLength(diElement: DIElement) =
    if (diElement.isNilled)
      isNillableKnownZeroLength(diElement)
    else
      isHexBinaryKnownNonZeroLength(diElement)
}
