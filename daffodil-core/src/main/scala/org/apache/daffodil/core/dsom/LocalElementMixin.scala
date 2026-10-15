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

package org.apache.daffodil.core.dsom

import java.math.BigInteger as JBigInt

import org.apache.daffodil.core.grammar.LocalElementGrammarMixin
import org.apache.daffodil.lib.equality.*
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.*
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType

/**
 * Common to local element decls and element references
 */
trait LocalElementMixin extends ParticleMixin with LocalElementGrammarMixin {
  self: ElementBase =>

  /**
   * True if the length of the SimpleContent region or the ComplexContent region
   * (see DFDL Spec section 9.2) is known to be greater than zero.
   *
   * These content grammar regions are orthogonal to both nillable representations, and
   * empty representations, and to all aspects of framing - alignment, skip, delimiters
   * etc.
   */
  final lazy val isContentRegionLengthKnownToBeGreaterThanZero =
    LV(Symbol("isContentRegionLengthKnownToBeGreaterThanZero")) {
      val pt = primType
      val res = lengthKind match {
        case LengthKind.Explicit => (isFixedLength && (fixedLength > 0))
        case LengthKind.Prefixed => false
        case LengthKind.Pattern =>
          lengthPattern.r.findFirstIn("") match { // can regex match nothing?
            case None => true
            case Some(s) => false
          }
        case LengthKind.Delimited =>
          (pt != PrimType.String && // delimited strings can be zero length
          pt != PrimType.HexBinary) // delimited hexBinary can be zero length
        case LengthKind.Implicit => {
          if (
            (pt =:= PrimType.String || pt =:= PrimType.HexBinary) && self.hasMaxLength && self.maxLength.toBigInteger
              .compareTo(JBigInt.ZERO) == 1
          ) true
          else if (representation =:= Representation.Binary) true
          else false
        }
        case LengthKind.EndOfParent if isComplexType =>
          notYetImplemented("lengthKind='endOfParent' for complex type")
        case LengthKind.EndOfParent =>
          notYetImplemented("lengthKind='endOfParent' for simple type")
      }
      res
    }.value

  final override def hasKnownRequiredSyntax: Boolean = !couldBeMissing

  final lazy val couldBeMissing: Boolean = LV(Symbol("couldBeMissing")) {
    val res =
      if (minOccurs == 0) true
      else if (isNillable && !hasNilValueRequiredSyntax) true
      else if (isDefaultable && emptyValueDelimiterPolicy =:= EmptyValueDelimiterPolicy.None)
        true
      else if (
        isDefaultable && !hasInitiator && emptyValueDelimiterPolicy =:= EmptyValueDelimiterPolicy.Initiator
      ) true
      else if (
        isDefaultable && !hasTerminator && emptyValueDelimiterPolicy =:= EmptyValueDelimiterPolicy.Terminator
      ) true
      // A delimiter known to occupy bits makes this element present. One that
      // matches nothing says nothing either way, so fall through to the content
      // analysis below. That is the mil-std-2045 case.
      else if (isKnownNonZeroLengthDelimiters) false
      else if (isSimpleType) {
        primType match {
          case PrimType.String | PrimType.HexBinary =>
            !isContentRegionLengthKnownToBeGreaterThanZero
          case PrimType.Boolean =>
            false // Note that textBooleanTrueRep and textBooleanFalseRep cannot contain %ES; so there is no way for an ES as boolean rep.
          case _ => false // all other types require some syntax.
        }
      } else if (isComplexType) {
        !complexType.group.hasKnownRequiredSyntax
      } else Assert.impossibleCase()
    res
  }.value

}
