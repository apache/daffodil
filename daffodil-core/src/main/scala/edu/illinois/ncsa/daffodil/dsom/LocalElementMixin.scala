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

import scala.xml.Node
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.api.WithDiagnostics
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG._
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.LV
import scala.util.matching.Regex
import edu.illinois.ncsa.daffodil.dsom.Facet._
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.grammar.LocalElementGrammarMixin

/**
 * Common to local element decls and element references
 */
trait LocalElementMixin
  extends ParticleMixin
  with LocalComponentMixin
  with LocalElementGrammarMixin { self: LocalElementBase =>

  lazy val hasSep = hasSep_.value
  private val hasSep_ = LV('hasSep) {
    nearestEnclosingSequence match {
      case None => false
      case Some(es) => {
        val res =
          es.separator.isKnownNonEmpty
        res
      }
    }
  }

  lazy val isDeclaredLastInSequence = isDeclaredLastInSequence_.value
  private val isDeclaredLastInSequence_ = LV('isDeclaredLastInSequence) {
    val es = nearestEnclosingSequence
    // how do we determine what child node we are? We search. 
    // TODO: better structure for O(1) answer to this.
    es match {
      case None => Assert.invariantFailed("We are not in a sequence therefore isDeclaredLastInSequence is an invalid question.")
      case Some(s) => {
        val members = s.groupMembersNoRefs
        if (members.last eq thisTermNoRefs) true // we want object identity comparison here, not equality. 
        else false
      }
    }
  }

  lazy val lengthKnownToBeGreaterThanZero = lengthKnownToBeGreaterThanZero_.value
  private val lengthKnownToBeGreaterThanZero_ = LV('lengthKnownToBeGreaterThanZero) {
    val pt = {
      if (isPrimType) typeDef.asInstanceOf[PrimType]
      else {
        val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
        st.primitiveType
      }
    }
    val res = lengthKind match {
      case LengthKind.Explicit => (isFixedLength && (fixedLength > 0))
      case LengthKind.Prefixed => false
      case LengthKind.Pattern => lengthPattern.r.findFirstIn("") match {
        case None => true
        case Some(s) => false
      }
      case LengthKind.Delimited => (pt == PrimType.String)
      case LengthKind.Implicit => false
      case LengthKind.EndOfParent => false
    }
    res
  }

  override lazy val hasKnownRequiredSyntax = hasKnownRequiredSyntax_.value
  private val hasKnownRequiredSyntax_ = LV('hasKnownRequiredSyntax) {
    if ((minOccurs > 0) || isScalar || isFixedOccurrences) {
      if (emptyValueDelimiterPolicy == EmptyValueDelimiterPolicy.None) true
      else if (emptyIsAnObservableConcept) true
      else {
        val pt = {
          if (isPrimType) typeDef.asInstanceOf[PrimType]
          else {
            val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
            st.primitiveType
          }
        }
        ((pt == PrimType.String) || (pt == PrimType.HexBinary) && lengthKnownToBeGreaterThanZero)
      }
    } else false
  }

  lazy val isLastDeclaredRequiredElementOfSequence = isLastDeclaredRequiredElementOfSequence_.value
  private val isLastDeclaredRequiredElementOfSequence_ = LV('isLastDeclaredRequiredElementOfSequence) {
    if (hasKnownRequiredSyntax) {
      val es = nearestEnclosingSequence
      es match {
        case None => true
        case Some(s) =>
          if (s.groupMembers.filter(_.hasKnownRequiredSyntax).last eq this) true
          else false
      }
      // Since we can't determine at compile time, return true so that we can continue processing.
      // Runtime checks will make final determination.
    } else true
  }

  lazy val separatorSuppressionPolicy = separatorSuppressionPolicy_.value
  private val separatorSuppressionPolicy_ = LV('separatorSuppressionPolicy) {
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
  }
}

