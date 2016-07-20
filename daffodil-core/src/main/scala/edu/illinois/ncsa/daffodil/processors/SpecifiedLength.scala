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

import java.util.regex.PatternSyntaxException

import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.processors.parsers._
import edu.illinois.ncsa.daffodil.processors.unparsers._
import edu.illinois.ncsa.daffodil.exceptions.Assert

abstract class SpecifiedLengthCombinatorBase(val e: ElementBase, eGram: => Gram)
    extends Terminal(e, true) {

  lazy val eParser = eGram.parser
  lazy val eUnparser = eGram.unparser

  def kind: String

  def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<SpecifiedLengthCombinator_" + kind + ">" +
        eParser.toBriefXML(depthLimit - 1) +
        "</SpecifiedLengthCombinator_" + kind + ">"
  }

}

class SpecifiedLengthPattern(e: ElementBase, eGram: => Gram)
    extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "Pattern"

  lazy val pattern =
    try {
      e.lengthPattern.r.pattern // imagine a really big expensive pattern to compile.
    } catch {
      case x: PatternSyntaxException =>
        e.SDE(x)
    }

  if (!e.encodingInfo.isScannable) e.SDE("Element %s does not meet the requirements of Pattern-Based lengths and Scanability.\nThe element and its children must be representation='text' and share the same encoding.", e.prettyName)

  override lazy val parser: Parser = new SpecifiedLengthPatternParser(
    eParser,
    e.elementRuntimeData,
    pattern)

  // When unparsing, the pattern is not used to calculate a length, so just
  // skip that parser and go straight to unparsing the string in the eUnparser
  override lazy val unparser: Unparser = eUnparser

}

class SpecifiedLengthExplicit(e: ElementBase, eGram: => Gram, bitsMultiplier: Int)
    extends SpecifiedLengthCombinatorBase(e, eGram) {

  Assert.usage(bitsMultiplier > 0)

  lazy val kind = "Explicit_" + e.lengthUnits.toString

  lazy val parser: Parser = new SpecifiedLengthExplicitParser(
    eParser,
    e.elementRuntimeData,
    e.lengthEv,
    bitsMultiplier)

  lazy val unparser: Unparser = new SpecifiedLengthExplicitUnparser(
    eUnparser,
    e.elementRuntimeData,
    e.lengthEv,
    bitsMultiplier)

}

class SpecifiedLengthImplicit(e: ElementBase, eGram: => Gram, nBits: Long)
    extends SpecifiedLengthCombinatorBase(e, eGram) {

  lazy val kind = "Implicit_" + e.lengthUnits.toString

  lazy val toBits = 1

  lazy val parser: Parser = new SpecifiedLengthImplicitParser(
    eParser,
    e.elementRuntimeData,
    nBits)

  lazy val unparser: Unparser = new SpecifiedLengthImplicitUnparser(
    eUnparser,
    e.elementRuntimeData,
    nBits)

}

class SpecifiedLengthExplicitCharacters(e: ElementBase, eGram: => Gram)
    extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "ExplicitCharacters"

  lazy val parser: Parser = new SpecifiedLengthExplicitCharactersParser(
    eParser,
    e.elementRuntimeData,
    e.lengthEv)

  lazy val unparser: Unparser = new SpecifiedLengthExplicitCharactersUnparser(
    eUnparser,
    e.elementRuntimeData,
    e.lengthEv)
}

class SpecifiedLengthImplicitCharacters(e: ElementBase, eGram: => Gram, nChars: Long)
    extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "ImplicitCharacters"

  lazy val parser: Parser = new SpecifiedLengthImplicitCharactersParser(
    eParser,
    e.elementRuntimeData,
    nChars)

  lazy val unparser: Unparser = new SpecifiedLengthImplicitCharactersUnparser(
    eUnparser,
    e.elementRuntimeData,
    nChars)
}
