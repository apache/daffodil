/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.grammar
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.dsom.DFDLNewVariableInstance
import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.dsom.Term
import org.apache.daffodil.grammar.primitives.OptionalInfixSep
import org.apache.daffodil.grammar.primitives.Nada
import org.apache.daffodil.grammar.primitives.MandatoryTextAlignment
import org.apache.daffodil.grammar.primitives.Separator

/////////////////////////////////////////////////////////////////
// Groups System
/////////////////////////////////////////////////////////////////

trait TermGrammarMixin
  extends AlignedMixin
  with BitOrderMixin { self: Term =>

  override protected final def grammarContext = this

  def termContentBody: Gram

  private lazy val newVars = this.annotationObjs.filter { st =>
    st.isInstanceOf[DFDLNewVariableInstance]
  }.asInstanceOf[Seq[DFDLNewVariableInstance]]

  private lazy val newVarStarts = newVars.map { _.gram }
  private lazy val newVarEnds = newVars.map { _.endGram }

  protected lazy val hasEncoding = optionEncodingRaw.isDefined

  // TODO: replace dfdlScopeBegin and dfdlScopeEnd with a single Combinator.
  protected final lazy val dfdlScopeBegin = prod("dfdlScopeBegin", newVarStarts.length > 0) {
    newVarStarts.fold(mt) { _ ~ _ }
  }

  protected final lazy val dfdlScopeEnd = prod("dfdlScopeEnd", newVarEnds.length > 0) {
    newVarEnds.fold(mt) { _ ~ _ }
  }

  // I am not sure we need to distinguish these two.
  final lazy val asTermInSequence = prod("asTermInSequence") {
    separatedForSequencePosition(termContentBody)
  }

  /**
   * overridden in LocalElementGrammarMixin
   */
  lazy val asTermInChoice = termContentBody

  /**
   * separator combinators - detect cases where no separator applies.
   * Note that repeating elements are excluded because they have to
   * managed their own separatedForArrayPosition inside the repetition.
   */
  protected final def separatedForArrayPosition(bodyArg: => Gram): Gram = {
    val body = bodyArg
    val (isElementWithNoRep, isRepeatingElement) = body.context match {
      case e: ElementBase => (!e.isRepresented, !e.isScalar)
      case other => (false, false)
    }
    Assert.usage(isRepeatingElement)
    Assert.invariant(!isElementWithNoRep) //inputValueCalc not allowed on arrays in DFDL v1.0
    val res = prefixSep ~ infixSepRule ~ body ~ postfixSep
    res
  }

  protected final def separatedForSequencePosition(bodyArg: => Gram): Gram = {
    val body = bodyArg
    val (isElementWithNoRep, isRepeatingElement) = body.context match {
      case e: ElementBase => (!e.isRepresented, !e.isScalar)
      case other => (false, false)
    }
    if (isElementWithNoRep) body // no separators for things that have no representation in the data stream
    else if (isRepeatingElement) body
    else {
      val res = prefixSep ~ infixSepRule ~ body ~ postfixSep
      res
    }
  }

  // public for unit testing use.
  final lazy val Some(es) = {
    //
    // Not sure how to assert this,
    // but an invariant we're assuming here is that we are NOT the
    // root element, which has no enclosing sequence at all.
    //
    // The grammar rules shouldn't be asking for separated stuff
    // in that situation, so we shouldn't be here.
    //
    // TODO: FIXME:
    // Also note: we can get away with just looking upward for nearest enclosing
    // sequence because we have restrictions on what can be inside a choice,
    // and we disallow delimiters on choices. If one allows delimiters on
    // choices... consider
    // <sequence dfdl:separator=",">
    //   <choice dfdl:initiator="[", terminator="]">
    //     <element ref="foo" maxOccurs="20"/>
    //     ...
    // In this case, what separates the multiple occurrances of foo? I claim
    // they are comma separated.
    // But data could be like this 'a, b, c,[foo1,foo2,foo3],d,e,f'
    //
    // Not unreasonable, but just too much complexity. Postpone until later.

    //
    // TODO: fix this when those restrictions are lifted.
    //
    subset(hasES, "(Current restriction) There must be an enclosing sequence.")
    nearestEnclosingSequence
  }

  private def hasES = nearestEnclosingSequence != None
  private def ignoreES = inChoiceBeforeNearestEnclosingSequence == true

  private lazy val separatorItself = prod("separator", !ignoreES && hasES) {
    //
    // TODO: (JIRA DFDL-1400) The separators may be in a different encoding than the terms
    // that they separate.
    //
    // So we must allow for a change of encoding (which may also imply a change
    // of bit order)
    //
    // However, this isn't the same as just plopping down a bitOrderChange ~ encodingChange, since
    // those examine prior peer, and what we want to scrutinize is the prior term being separated.
    //
    delimMTA ~ Separator(es, self)
  }

  private lazy val sepRule = separatorItself

  private lazy val prefixSep = prod("prefixSep", !ignoreES && hasES && es.hasPrefixSep) {
    sepRule
  }

  private lazy val postfixSep = prod("postfixSep", !ignoreES && hasES && es.hasPostfixSep) { sepRule }
  private lazy val infixSep = prod("infixSep", !ignoreES && hasES && es.hasInfixSep) { sepRule }

  private lazy val isStaticallyFirst = {
    es.hasInfixSep &&
      this.positionInNearestEnclosingSequence == 1 &&
      isScalar &&
      !hasPriorRequiredSiblings
  }

  private lazy val infixSepRule = prod("infixSepRule", !ignoreES && hasES && es.hasInfixSep) {
    if (isStaticallyFirst) Nada(this) // we're first, no infix sep.
    else if (hasPriorRequiredSiblings) infixSep // always in this case
    else if (positionInNearestEnclosingSequence > 1 || !isScalar) {
      new OptionalInfixSep(this, infixSep)
    } else Assert.invariantFailed("infixSepRule didn't understand what to lay down as grammar for this situation: " + this)
  }

  /**
   * Mandatory text alignment or mta
   *
   * mta can only apply to things with encodings. No encoding, no MTA.
   *
   * In addition, it has to be textual data. Just because there's an encoding
   * in the property environment shouldn't get you an MTA region. It has
   * to be textual.
   */
  protected lazy val mtaBase = prod("mandatoryTextAlignment", hasEncoding) {
    MandatoryTextAlignment(this, knownEncodingAlignmentInBits, false)
  }

  /**
   * Mandatory text alignment for delimiters
   */
  protected lazy val delimMTA = prod("delimMTA",
    {
      hasDelimiters
    }) {
      // This is different from mtaBase because it passes in 'true' for the
      // last parameter to signify that it is MTA for a delimiter. mtaBase
      // passes in 'false'
      MandatoryTextAlignment(this, knownEncodingAlignmentInBits, true)
    }

}
