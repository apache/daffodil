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

package org.apache.daffodil.grammar.primitives

import java.util.regex.PatternSyntaxException
import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.grammar.Gram
import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.processors.parsers._
import org.apache.daffodil.processors.unparsers._
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.unparsers.SpecifiedLengthExplicitImplicitUnparser
import org.apache.daffodil.processors.parsers.SpecifiedLengthExplicitParser
import org.apache.daffodil.processors.parsers.SpecifiedLengthImplicitParser
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.processors.parsers.Parser

abstract class SpecifiedLengthCombinatorBase(val e: ElementBase, eGramArg: => Gram)
  extends Terminal(e, true) {

  lazy val eGram = eGramArg // once only
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

  if (!e.encodingInfo.isScannable && !(e.isSimpleType && e.primType == PrimType.HexBinary)) {
    // lengthKind="pattern" requires scanability. However, xs:hexBinary types
    // are not scannable, but we do allow pattern lengths if the encoding is
    // ISO-8859-1. The ISO-8859-1 check is done elsewhere, so to allow pattern
    // lengths, either this must be scannable or the type must be xs:hexBinary.
    // Anything else is an error.
    e.SDE("Element %s does not meet the requirements of Pattern-Based lengths and Scanability.\nThe element and its children must be representation='text' and share the same encoding.", e.diagnosticDebugName)
  }

  override lazy val parser: Parser = new SpecifiedLengthPatternParser(
    eParser,
    e.elementRuntimeData,
    pattern)

  // When unparsing, the pattern is not used to calculate a length, so just
  // skip that parser and go straight to unparsing the string in the eUnparser
  override lazy val unparser: Unparser = {
    pattern
    // force pattern just so we detect regex syntax errors even though
    // we don't use the pattern when unparsing.
    eUnparser
  }
}

trait SpecifiedLengthExplicitImplicitUnparserMixin {

  def e: ElementBase
  def eUnparser: Unparser

  lazy val unparser: Unparser = new SpecifiedLengthExplicitImplicitUnparser(
    eUnparser,
    e.elementRuntimeData,
    e.unparseTargetLengthInBitsEv,
    e.maybeUnparseTargetLengthInCharactersEv)
}

class SpecifiedLengthExplicit(e: ElementBase, eGram: => Gram, bitsMultiplier: Int)
  extends SpecifiedLengthCombinatorBase(e, eGram)
  with SpecifiedLengthExplicitImplicitUnparserMixin {

  Assert.usage(bitsMultiplier > 0)

  lazy val kind = "Explicit_" + e.lengthUnits.toString

  lazy val parser: Parser = new SpecifiedLengthExplicitParser(
    eParser,
    e.elementRuntimeData,
    e.lengthEv,
    bitsMultiplier)

}

class SpecifiedLengthImplicit(e: ElementBase, eGram: => Gram, nBits: Long)
  extends SpecifiedLengthCombinatorBase(e, eGram)
  with SpecifiedLengthExplicitImplicitUnparserMixin {

  lazy val kind = "Implicit_" + e.lengthUnits.toString

  lazy val toBits = 1

  lazy val parser: Parser = new SpecifiedLengthImplicitParser(
    eParser,
    e.elementRuntimeData,
    nBits)

}

class SpecifiedLengthExplicitCharacters(e: ElementBase, eGram: => Gram)
  extends SpecifiedLengthCombinatorBase(e, eGram)
  with SpecifiedLengthExplicitImplicitUnparserMixin {

  val kind = "ExplicitCharacters"

  lazy val parser: Parser = new SpecifiedLengthExplicitCharactersParser(
    eParser,
    e.elementRuntimeData,
    e.lengthEv)
}

class SpecifiedLengthImplicitCharacters(e: ElementBase, eGram: => Gram, nChars: Long)
  extends SpecifiedLengthCombinatorBase(e, eGram)
  with SpecifiedLengthExplicitImplicitUnparserMixin {

  val kind = "ImplicitCharacters"

  lazy val parser: Parser = new SpecifiedLengthImplicitCharactersParser(
    eParser,
    e.elementRuntimeData,
    nChars)
}
