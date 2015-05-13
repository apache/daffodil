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
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import java.nio.charset.Charset
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthUnits
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.dpath.AsIntConverters
import edu.illinois.ncsa.daffodil.processors.parsers._

abstract class SpecifiedLengthCombinatorBase(val e: ElementBase, eGram: => Gram)
  extends Terminal(e, true) {

  val eParser = eGram.parser

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

  if (!e.encodingInfo.isScannable) e.SDE("Element %s does not meet the requirements of Pattern-Based lengths and Scanability.\nThe element and its children must be representation='text' and share the same encoding.", e.prettyName)
  def parser: Parser = new SpecifiedLengthPatternParser(
    eParser,
    e.elementRuntimeData,
    e.lengthPattern)

}

class SpecifiedLengthExplicitBitsFixed(e: ElementBase, eGram: => Gram, nBits: Long)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "ExplicitBitsFixed"

  def parser: Parser = new SpecifiedLengthExplicitBitsFixedParser(
    eParser,
    e.elementRuntimeData,
    nBits,
    e.knownEncodingCharset)

}

class SpecifiedLengthExplicitBits(e: ElementBase, eGram: => Gram)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "ExplicitBits"

  lazy val toBits = e.lengthUnits match {
    case LengthUnits.Bits => 1
    case LengthUnits.Bytes => 8
    case _ => e.schemaDefinitionError("Binary Numbers must have length units of Bits or Bytes.")
  }

  def parser: Parser = new SpecifiedLengthExplicitBitsParser(
    eParser,
    e.elementRuntimeData,
    e.knownEncodingCharset,
    e.length,
    toBits)

}

class SpecifiedLengthExplicitBytesFixed(e: ElementBase, eGram: => Gram, nBytes: Long)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "ExplicitBytesFixed"

  def parser: Parser = new SpecifiedLengthExplicitBytesFixedParser(
    eParser,
    e.elementRuntimeData,
    nBytes,
    e.knownEncodingCharset)

}

class SpecifiedLengthExplicitBytes(e: ElementBase, eGram: => Gram)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "ExplicitBytes"

  def parser: Parser = new SpecifiedLengthExplicitBytesParser(
    eParser,
    e.elementRuntimeData,
    e.knownEncodingCharset,
    e.length)

}

class SpecifiedLengthExplicitCharactersFixed(e: ElementBase, eGram: => Gram, nChars: Long)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "ExplicitCharactersFixed"

  def parser: Parser = new SpecifiedLengthExplicitCharactersFixedParser(
    eParser,
    e.elementRuntimeData,
    nChars)

}

class SpecifiedLengthExplicitCharacters(e: ElementBase, eGram: => Gram)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "ExplicitCharacters"

  def parser: Parser = new SpecifiedLengthExplicitCharactersParser(
    eParser,
    e.elementRuntimeData,
    e.length)

}

