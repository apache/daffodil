package edu.illinois.ncsa.daffodil.dsom

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import scala.xml.Node
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.util._
import com.ibm.icu.text.NumberFormat
import java.math.BigInteger

/////////////////////////////////////////////////////////////////
// Groups System
/////////////////////////////////////////////////////////////////

trait TermGrammarMixin extends BitOrderMixin { self: Term =>

  lazy val newVars = this.annotationObjs.filter { st =>
    st.isInstanceOf[DFDLNewVariableInstance]
  }.asInstanceOf[Seq[DFDLNewVariableInstance]]

  lazy val newVarStarts = newVars.map { _.gram }
  lazy val newVarEnds = newVars.map { _.endGram }

  // TODO: replace dfdlScopeBegin and dfdlScopeEnd with a single Combinator.
  lazy val dfdlScopeBegin = Prod("dfdlScopeBegin", this, newVarStarts.length > 0,
    newVarStarts.fold(EmptyGram) { _ ~ _ })

  lazy val dfdlScopeEnd = Prod("dfdlScopeEnd", this, newVarEnds.length > 0,
    newVarEnds.fold(EmptyGram) { _ ~ _ })

  def termContentBody: Prod

  // I am not sure we need to distinguish these two. 
  lazy val asTermInSequence = separatedForSequencePosition(termContentBody)
  lazy val asTermInChoice = termContentBody

  /**
   * separator combinators - detect cases where no separator applies.
   * Note that repeating elements are excluded because they have to
   * managed their own separatedForArrayPosition inside the repetition.
   */
  def separatedForArrayPosition(body: => Gram) = {
    val (isElementWithNoRep, isRepeatingElement) = body.context match {
      case e: ElementBase => (!e.isRepresented, !e.isScalar)
      case other => (false, false)
    }
    Assert.usage(isRepeatingElement)
    Assert.invariant(!isElementWithNoRep) //inputValueCalc not allowed on arrays in DFDL v1.0
    val res = prefixSep ~ infixSepRule ~ body ~ postfixSep
    res
  }

  def separatedForSequencePosition(body: => Gram) = {
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

  lazy val Some(es) = {
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

  def hasES = nearestEnclosingSequence != None
  def ignoreES = inChoiceBeforeNearestEnclosingSequence == true

  lazy val staticSeparator = Prod("staticSeparator", this, !ignoreES && hasES && es.separator.isConstant,
    StaticSeparator(es, self))

  lazy val dynamicSeparator = Prod("dynamicSeparator", this, !ignoreES && hasES && !es.separator.isConstant,
    DynamicSeparator(es, self))

  lazy val sepRule = staticSeparator | dynamicSeparator

  lazy val prefixSep = Prod("prefixSep", this,
    {
      val res = !ignoreES && hasES && es.hasPrefixSep
      res
    },
    sepRule)

  lazy val postfixSep = Prod("postfixSep", this, !ignoreES && hasES && es.hasPostfixSep, sepRule)
  lazy val infixSep = Prod("infixSep", this, !ignoreES && hasES && es.hasInfixSep, sepRule)

  lazy val isStaticallyFirst = {
    es.hasInfixSep &&
      this.positionInNearestEnclosingSequence == 1 &&
      isScalar &&
      !hasPriorRequiredSiblings
  }

  lazy val infixSepRule = Prod("infixSepRule", this,
    !ignoreES && hasES && es.hasInfixSep, {
      if (isStaticallyFirst) Nada(this) // we're first, no infix sep.
      else if (hasPriorRequiredSiblings) infixSep // always in this case
      else if (positionInNearestEnclosingSequence > 1 || !isScalar) {
        new OptionalInfixSep(this, infixSep)
      } else Assert.invariantFailed("infixSepRule didn't understand what to lay down as grammar for this situation: " + this)
    })

}

