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
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthUnits
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.dpath.AsIntConverters

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

abstract class SpecifiedLengthParserBase(eParser: Parser,
  erd: ElementRuntimeData)
  extends Parser(erd)
  with WithParseErrorThrowing {

  override lazy val childProcessors = Seq(eParser)

  final def parse(pstate: PState, endBitPos: Long) = {
    log(LogLevel.Debug, "Limiting data to %s bits.", endBitPos)
    val savedLimit = pstate.bitLimit0b
    val startPos0b = pstate.bitPos0b
    pstate.setEndBitLimit(endBitPos)
    eParser.parse1(pstate, erd)
    val limitedLength = endBitPos - startPos0b
    val endOfChildrenPos0b = pstate.bitPos0b
    val childrenLength = endOfChildrenPos0b - startPos0b

    log(LogLevel.Debug, "Restoring data limit to %s bits.", pstate.bitLimit0b)

    pstate.setEndBitLimit(savedLimit)
    pstate.status match {
      case Success => {
        // Check that the parsed length is less than or equal to the length of the parent
        //Assert.invariant(postState2.bitPos <= endBitPos)
        this.PECheck(pstate.bitPos <= endBitPos, "The parsed length of the children (%s bits) was greater than that of the parent (%s bits).", childrenLength, limitedLength)
        pstate.setPos(endBitPos, -1, Nope)
      }
      case _ => //ok
    }
  }

}

class SpecifiedLengthPatternParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  pattern: String)
  extends SpecifiedLengthParserBase(eParser, erd) {

  @transient lazy val d = new ThreadLocal[DFDLDelimParser] {
    override def initialValue() = {
      new DFDLDelimParser(erd)
    }
  }

  def parse(start: PState): Unit = withParseErrorThrowing(start) {
    val in = start.inStream

    val reader = in.getCharReader(erd.encodingInfo.knownEncodingCharset.charset, start.bitPos)

    val result = d.get.parseInputPatterned(pattern, reader, start)

    val endBitPos =
      result match {
        case _: DelimParseFailure => start.bitPos + 0 // no match == length is zero!
        case s: DelimParseSuccess => start.bitPos + s.numBits
      }
    parse(start, endBitPos)
  }
}

class SpecifiedLengthExplicitBitsParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  dcharset: DFDLCharset,
  length: CompiledExpression,
  toBits: Int)
  extends SpecifiedLengthParserBase(eParser, erd) {

  // TODO: These SpecifiedLength* classes need some refactorization. This
  // function and getLength are all copied in numerous places 

  def getBitLength(s: PState): Long = {
    val nBytesAsAny = length.evaluate(s)
    val nBytes = AsIntConverters.asLong(nBytesAsAny)
    nBytes * toBits
  }

  def parse(pState: PState): Unit = withParseErrorThrowing(pState) {

    val nBits = getBitLength(pState)
    val in = pState.inStream

    val startBitPos = pState.bitPos
    try {
      val nBytes = scala.math.ceil(nBits / 8.0).toLong
      val bytes = in.getBytes(pState.bitPos, nBytes)
      val endBitPos = pState.bitPos + nBits
      parse(pState, endBitPos)
    } catch {
      case ex: IndexOutOfBoundsException => {
        // Insufficient bytes in field, but we need to still allow processing
        // to test for Nils
        val endBitPos = startBitPos + 0
        parse(pState, endBitPos)
      }
      case u: UnsuppressableException => throw u
      case e: Exception => {
        PE(pState, "SpecifiedLengthExplicitBitsParser - Exception: \n%s", e.getStackTraceString)
      }
    }
  }
}

class SpecifiedLengthExplicitBitsFixedParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  nBits: Long,
  dcharset: DFDLCharset)
  extends SpecifiedLengthParserBase(eParser, erd) {

  def parse(start: PState): Unit = withParseErrorThrowing(start) {

    val in = start.inStream

    try {
      val nBytes = scala.math.ceil(nBits / 8.0).toLong
      val bytes = in.getBytes(start.bitPos, nBytes)
      val endBitPos = start.bitPos + nBits
      parse(start, endBitPos)
    } catch {
      case ex: IndexOutOfBoundsException => {
        // Insufficient bits in field, but we need to still allow processing
        // to test for Nils
        val endBitPos = start.bitPos + 0
        parse(start, endBitPos)
      }
      case u: UnsuppressableException => throw u
      case ex: Exception => { PE(start, "SpecifiedLengthExplicitBitsFixedParser - Exception: \n%s", ex.getStackTraceString) }
    }
  }
}

class SpecifiedLengthExplicitBytesParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  dcharset: DFDLCharset,
  length: CompiledExpression)
  extends SpecifiedLengthParserBase(eParser, erd) {

  def getLength(s: PState): Long = {
    val nBytesAsAny = length.evaluate(s)
    val nBytes = AsIntConverters.asLong(nBytesAsAny)
    nBytes
  }

  def parse(pState: PState): Unit = withParseErrorThrowing(pState) {
    val limit = pState.bitLimit0b
    val lenAvailable = pState.bitLimit0b - pState.bitPos0b
    val nBytes = getLength(pState)
    val in = pState.inStream
    val bytes = try {
      in.getBytes(pState.bitPos, nBytes)
    } catch {
      case ex: IndexOutOfBoundsException => {
        PE("Insufficient data. Required %s bytes.%s", nBytes,
          {

            if (limit != -1) " Only %s bytes were available.".format(scala.math.ceil(lenAvailable / 8.0).toLong)
            else ""
          })
        // Insufficient bytes in field, but we need to still allow processing
        // to test for Nils
        //        val endBitPos = start.bitPos + 0
        //        val postEState = parse(start, endBitPos)
        //        return postEState
      }
    }
    val endBitPos = pState.bitPos + (nBytes * 8)
    parse(pState, endBitPos)
  }
}

class SpecifiedLengthExplicitBytesFixedParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  nBytes: Long,
  dcharset: DFDLCharset)
  extends SpecifiedLengthParserBase(eParser, erd) {

  def parse(start: PState): Unit = withParseErrorThrowing(start) {

    val in = start.inStream

    try {
      // val bytes = in.getBytes(start.bitPos, nBytes)
      val endBitPos = start.bitPos + (nBytes * 8)
      super.parse(start, endBitPos)
    } catch {
      case ex: IndexOutOfBoundsException => {
        // Insufficient bytes in field, but we need to still allow processing
        // to test for Nils
        val endBitPos = start.bitPos + 0
        super.parse(start, endBitPos)
      }
      case u: UnsuppressableException => throw u
    }
  }
}

class SpecifiedLengthExplicitCharactersFixedParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  nChars: Long)
  extends SpecifiedLengthParserBase(eParser, erd) {

  def parse(start: PState): Unit = withParseErrorThrowing(start) {

    val in = start.inStream
    val rdr = in.getCharReader(erd.encodingInfo.knownEncodingCharset.charset, start.bitPos)
    val field = rdr.getStringInChars(nChars.toInt).toString() // TODO: Don't we want getStringInChars to accept Long?!
    val fieldLength = field.length
    val endBitPos =
      if (fieldLength != nChars.toInt) start.bitPos + 0 // no match == length is zero!
      else {
        val numBits = erd.encodingInfo.knownEncodingStringBitLength(field)
        start.bitPos + numBits
      }
    parse(start, endBitPos)
  }
}

class SpecifiedLengthExplicitCharactersParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  length: CompiledExpression)
  extends SpecifiedLengthParserBase(eParser, erd) {

  def getLength(s: PState): Long = {
    val nBytesAsAny = length.evaluate(s)
    val nBytes = AsIntConverters.asLong(nBytesAsAny)
    nBytes
  }

  def parse(pState: PState): Unit = withParseErrorThrowing(pState) {

    val nChars = getLength(pState)
    val in = pState.inStream
    val rdr = in.getCharReader(erd.encodingInfo.knownEncodingCharset.charset, pState.bitPos)

    val field = rdr.getStringInChars(nChars.toInt).toString()
    val fieldLength = field.length
    val endBitPos =
      if (fieldLength != nChars.toInt) pState.bitPos + 0 // no match == length is zero!
      else {
        val numBits = erd.encodingInfo.knownEncodingStringBitLength(field)
        pState.bitPos + numBits
      }

    parse(pState, endBitPos)
  }
}

