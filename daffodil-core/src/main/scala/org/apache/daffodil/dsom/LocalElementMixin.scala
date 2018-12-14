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

package org.apache.daffodil.dsom

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.grammar._
import org.apache.daffodil.schema.annotation.props.gen._
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.grammar.LocalElementGrammarMixin
import org.apache.daffodil.equality._
import java.math.{ BigInteger => JBigInt }

/**
 * Common to local element decls and element references
 */
trait LocalElementMixin
  extends ParticleMixin
  with LocalElementGrammarMixin { self: ElementBase =>

  final def hasSep = LV('hasSep) {
    nearestEnclosingSequence match {
      case None => false
      case Some(es) => {
        val res = es.separatorParseEv.isKnownNonEmpty
        res
      }
    }
  }.value

  /**
   * True if the length of the SimpleContent region or the ComplexContent region
   * (see DFDL Spec section 9.2) is known to be greater than zero.
   *
   * These content grammar regions are orthogonal to both nillable representations, and
   * empty representations, and to all aspects of framing - alignment, skip, delimiters
   * etc.
   *
   */
  final def isContentRegionLengthKnownToBeGreaterThanZero = LV('isContentRegionLengthKnownToBeGreaterThanZero) {
    val pt = primType
    val res = lengthKind match {
      case LengthKind.Explicit => (isFixedLength && (fixedLength > 0))
      case LengthKind.Prefixed => false
      case LengthKind.Pattern => lengthPattern.r.findFirstIn("") match { // can regex match nothing?
        case None => true
        case Some(s) => false
      }
      case LengthKind.Delimited =>
        (pt != PrimType.String && // delimited strings can be zero length
          pt != PrimType.HexBinary) // delimited hexBinary can be zero length
      case LengthKind.Implicit => {
        if ((pt =:= PrimType.String || pt =:= PrimType.HexBinary) && self.hasMaxLength && self.maxLength.toBigInteger.compareTo(JBigInt.ZERO) == 1) true
        else if (representation =:= Representation.Binary) true
        else false
      }
      case LengthKind.EndOfParent if isComplexType => notYetImplemented("lengthKind='endOfParent' for complex type")
      case LengthKind.EndOfParent => notYetImplemented("lengthKind='endOfParent' for simple type")
    }
    res
  }.value

  final override def hasKnownRequiredSyntax: Boolean = LV('hasKnownRequiredSyntax) {
    !couldBeMissing
  }.value

  final def couldBeMissing: Boolean = LV('couldBeMissing) {
    val res =
      if (minOccurs == 0) true
      else if (isNillable && !hasNilValueRequiredSyntax) true
      else if (isDefaultable && emptyValueDelimiterPolicy =:= EmptyValueDelimiterPolicy.None) true
      else if (isDefaultable && !hasInitiator && emptyValueDelimiterPolicy =:= EmptyValueDelimiterPolicy.Initiator) true
      else if (isDefaultable && !hasTerminator && emptyValueDelimiterPolicy =:= EmptyValueDelimiterPolicy.Terminator) true
      else if (hasInitiator) false
      else if (hasTerminator) false
      else if (isSimpleType) {
        primType match {
          case PrimType.String | PrimType.HexBinary => !isContentRegionLengthKnownToBeGreaterThanZero
          case PrimType.Boolean => false // Note that textBooleanTrueRep and textBooleanFalseRep cannot contain %ES; so there is no way for an ES as boolean rep.
          case _ => false // all other types require some syntax.
        }
      } else if (isComplexType) {
        !complexType.group.hasKnownRequiredSyntax
      } else Assert.impossibleCase()
    res
  }.value

  final def isLastDeclaredRequiredElementOfSequence = LV('isLastDeclaredRequiredElementOfSequence) {
    if (isRequiredInInfoset) {
      val es = nearestEnclosingSequence
      val res = es match {
        case None => true
        case Some(s) => {
          val allRequired = s.groupMembers.filter(_.isRequiredInInfoset)
          val lastDeclaredRequired = allRequired.last
          if (lastDeclaredRequired _eq_ this) true
          else false
        }
      }
      res
      // Since we can't determine at compile time, return true so that we can continue processing.
      // Runtime checks will make final determination.
    } else true
  }.value

  final def separatorSuppressionPolicy = LV('separatorSuppressionPolicy) {
    nearestEnclosingSequence match {
      case Some(ssp) => ssp.separatorSuppressionPolicy
      //
      // There is no enclosing sequence (could be just a big nest of choices I suppose)
      // In that case, there still has to be a value for this.
      //
      // Strictly speaking, it's a bug to require this property in a situation where there
      // are no sequences at all. However, that's so unlikely to be the case that it's
      // not worth paying attention to. In any real format, the root element will be
      // a sequence.
      //
      case None => schemaDocument.separatorSuppressionPolicy
    }
  }.value
}
