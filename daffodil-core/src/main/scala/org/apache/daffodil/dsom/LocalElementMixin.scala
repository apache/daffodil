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

package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.grammar.LocalElementGrammarMixin
import edu.illinois.ncsa.daffodil.equality._
import java.math.{ BigInteger => JBigInt }

/**
 * Common to local element decls and element references
 */
trait LocalElementMixin
  extends ParticleMixin
  with LocalElementGrammarMixin { self: LocalElementBase =>

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
    if (isRequired) {
      val es = nearestEnclosingSequence
      val res = es match {
        case None => true
        case Some(s) => {
          val allRequired = s.groupMembersNoRefs.filter(_.isRequired)
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
