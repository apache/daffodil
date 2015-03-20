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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.grammar.NamedGram
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TestKind
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.dpath.DFDLCheckConstraintsFunction
import edu.illinois.ncsa.daffodil.api.ValidationMode
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.dpath.DFDLCheckConstraintsFunction
import edu.illinois.ncsa.daffodil.processors.unparsers.Unparser
import edu.illinois.ncsa.daffodil.processors.unparsers.StatementElementUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.StatementElementUnparserNoRep

class ElementCombinator(context: ElementBase, eGram: Gram, eAfterGram: Gram)
  extends ElementCombinatorBase(context, eGram, eAfterGram) {

  lazy val parser: Parser =
    if (context.isRepresented)
      new StatementElementParser(
        context.erd,
        context.name,
        patDiscrim,
        patAssert,
        pSetVar,
        testDiscrim,
        testAssert,
        eParser,
        eAfterParser)
    else
      new StatementElementParserNoRep(
        context.erd,
        context.name,
        patDiscrim,
        patAssert,
        pSetVar,
        testDiscrim,
        testAssert,
        eParser,
        eAfterParser)

  override lazy val unparser: Unparser =
    if (context.isRepresented)
      new StatementElementUnparser(context.erd, context.name, uSetVar, eUnparser, eAfterUnparser)
    else
      new StatementElementUnparserNoRep(context.erd, context.name, uSetVar, eUnparser, eAfterUnparser)
}

class ChoiceElementCombinator(context: ElementBase, eGram: Gram, eAfterGram: Gram)
  extends ElementCombinatorBase(context, eGram, eAfterGram) {

  def parser: Parser = new ChoiceStatementElementParser(
    context.erd,
    context.name,
    patDiscrim,
    patAssert,
    pSetVar,
    testDiscrim,
    testAssert,
    eParser,
    eAfterParser)

  override def unparser: Unparser = ???
  override lazy val eUnparser = ???
  override lazy val eAfterUnparser = ???
}

abstract class ElementCombinatorBase(context: ElementBase, eGram: Gram, eGramAfter: Gram)
  extends NamedGram(context) {

  // The order of things matters in some cases, so to be consistent we'll always use the 
  // same order even when it doesn't matter

  // The order of evaluation of statements is:
  // - pattern discriminators
  // - pattern asserts
  // - the parsing of the element itself
  // - setVariables
  // - test discriminators (must be attempted even if the parsing of element or setVariable statements fail)
  // - test asserts

  // requiredEvaluations(patDiscrim, patAssert, eGram, setVar, testDiscrim, testAssert)
  // Note: above not needed as these are ALWAYS evaluated below.

  lazy val patDiscrim = context.discriminatorStatements.filter(_.testKind == TestKind.Pattern).map(_.gram.parser)
  lazy val patAssert = context.assertStatements.filter(_.testKind == TestKind.Pattern).map(_.gram.parser)
  lazy val pSetVar = context.setVariableStatements.map(_.gram.parser)
  lazy val testDiscrim = context.discriminatorStatements.filter(_.testKind == TestKind.Expression).map(_.gram.parser)
  lazy val testAssert = context.assertStatements.filter(_.testKind == TestKind.Expression).map(_.gram.parser)

  lazy val eParser: Option[Parser] = if (eGram.isEmpty) None
  else Some(eGram.parser)

  lazy val eAfterParser: Option[Parser] =
    if (eGramAfter.isEmpty) None
    else Some(eGramAfter.parser)

  def parser: Parser

  lazy val uSetVar = context.setVariableStatements.map(_.gram.unparser)
  lazy val eUnparser: Option[Unparser] =
    if (eGram.isEmpty) None
    else Some(eGram.unparser)

  lazy val eAfterUnparser: Option[Unparser] =
    if (eGramAfter.isEmpty) None
    else Some(eGramAfter.unparser)

  def unparser: Unparser

}

