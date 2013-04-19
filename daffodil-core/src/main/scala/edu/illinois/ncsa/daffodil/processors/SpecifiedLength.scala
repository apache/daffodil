package edu.illinois.ncsa.daffodil.processors

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

import edu.illinois.ncsa.daffodil.dsom.DFDLAssert
import edu.illinois.ncsa.daffodil.dsom.DFDLDiscriminator
import edu.illinois.ncsa.daffodil.dsom.DFDLSetVariable
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.grammar.NamedGram
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TestKind
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.dsom.R
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextNumberJustification
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextTrimKind
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextStringJustification
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextBooleanJustification
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextCalendarJustification
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._

abstract class SpecifiedLengthCombinatorBase(val e: ElementBase, eGram: => Gram)
  extends Terminal(e, true)
  with RuntimeExplicitLengthMixin[Long] {
  //  extends NamedGram(e) {

  // requiredEvaluations(eGram) // Note: not really required for grammar objects. 
  // The eGram is only required if the grammar clause actually ends up spliced 
  // into the final grammar, and we can't tell that here, so whether the egram
  // ends up evaluated or not really has to happen in the application logic.

  val eParser = eGram.parser

  def kind: String

  def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<SpecifiedLength" + kind + ">" +
        eParser.toBriefXML(depthLimit - 1) +
        "</SpecifiedLength" + kind + ">"
  }

}

class SpecifiedLengthPattern(e: ElementBase, eGram: => Gram)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "Pattern"

  def parser: Parser = new SpecifiedLengthPatternParser(this, e)
  def unparser: Unparser = new DummyUnparser(e)

}

class SpecifiedLengthExplicitBitsFixed(e: ElementBase, eGram: => Gram, nBits: Long)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "ExplicitBitsFixed"

  def parser: Parser = new SpecifiedLengthExplicitBitsFixedParser(this, e, nBits)
  def unparser: Unparser = new DummyUnparser(e)

}

class SpecifiedLengthExplicitBits(e: ElementBase, eGram: => Gram)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "ExplicitBits"

  def parser: Parser = new SpecifiedLengthExplicitBitsParser(this, e)
  def unparser: Unparser = new DummyUnparser(e)

}

class SpecifiedLengthExplicitBytesFixed(e: ElementBase, eGram: => Gram, nBytes: Long)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "ExplicitBytesFixed"

  def parser: Parser = new SpecifiedLengthExplicitBytesFixedParser(this, e, nBytes)
  def unparser: Unparser = new DummyUnparser(e)

}

class SpecifiedLengthExplicitBytes(e: ElementBase, eGram: => Gram)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "ExplicitBytes"

  def parser: Parser = new SpecifiedLengthExplicitBytesParser(this, e)
  def unparser: Unparser = new DummyUnparser(e)

}

class SpecifiedLengthExplicitCharactersFixed(e: ElementBase, eGram: => Gram, nChars: Long)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "ExplicitCharactersFixed"

  def parser: Parser = new SpecifiedLengthExplicitCharactersFixedParser(this, e, nChars)
  def unparser: Unparser = new DummyUnparser(e)

}

class SpecifiedLengthExplicitCharacters(e: ElementBase, eGram: => Gram)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "ExplicitCharacters"

  def parser: Parser = new SpecifiedLengthExplicitCharactersParser(this, e)
  def unparser: Unparser = new DummyUnparser(e)

}

abstract class SpecifiedLengthParserBase(combinator: SpecifiedLengthCombinatorBase,
  e: ElementBase)
  extends PrimParser(combinator, e)
  with WithParseErrorThrowing {

  def toBriefXML = combinator.toBriefXML _

  final def parse(pstate: PState, endBitPos: Long, e: ElementBase) = {
    log(LogLevel.Debug, "Limiting data to %s bits.", endBitPos)
    val postState1 = pstate.withEndBitLimit(endBitPos)
    val postState2 = combinator.eParser.parse1(postState1, e)
    
    log(LogLevel.Debug, "Restoring data limit to %s bits.", pstate.bitLimit)
    
    val postState3 = postState2.withEndBitLimit(pstate.bitLimit)
    val finalState = postState3.status match {
      case Success => {
        // Check that the parsed length is less than or equal to the length of the parent
        //Assert.invariant(postState2.bitPos <= endBitPos)
        this.PECheck(postState2.bitPos <= endBitPos, "The parsed length of the children was greater than that of the parent.")
        postState3.withPos(endBitPos, -1, None)
      }
      case _ => postState3
    }
    finalState
  }

}

class SpecifiedLengthPatternParser(combinator: SpecifiedLengthCombinatorBase, e: ElementBase)
  extends SpecifiedLengthParserBase(combinator, e) {

  val charset = e.knownEncodingCharset
  val pattern = e.lengthPattern

  def parse(start: PState): PState = withParseErrorThrowing(start) {
    val in = start.inStream

    val reader = in.getCharReader(charset, start.bitPos)
    val d = new DelimParser(e.knownEncodingStringBitLengthFunction)
    val result = d.parseInputPatterned(pattern, reader)
    val endBitPos =
      result match {
        case _: DelimParseFailure => start.bitPos + 0 // no match == length is zero!
        case s: DelimParseSuccess => start.bitPos + s.numBits
      }
    val postEState = parse(start, endBitPos, e)
    postEState
  }
}

class SpecifiedLengthExplicitBitsParser(combinator: SpecifiedLengthCombinatorBase, e: ElementBase)
  extends SpecifiedLengthParserBase(combinator, e) {

  val charset = e.knownEncodingCharset
  val expr = e.length

  def parse(start: PState): PState = withParseErrorThrowing(start) {

    val (pState, nBits) = combinator.getBitLength(start)
    val in = pState.inStream

    try {
      val nBytes = scala.math.ceil(nBits / 8.0).toLong
      val bytes = in.getBytes(pState.bitPos, nBytes)
      val endBitPos = pState.bitPos + nBits
      val postEState = parse(pState, endBitPos, e)
      return postEState
    } catch {
      case ex: IndexOutOfBoundsException => {
        // Insufficient bytes in field, but we need to still allow processing
        // to test for Nils
        val endBitPos = start.bitPos + 0
        val postEState = parse(start, endBitPos, e)
        return postEState
      }
      case u: UnsuppressableException => throw u
      case e: Exception => { return PE(pState, "SpecifiedLengthExplicitBitsParser - Exception: \n%s", e.getStackTraceString) }
    }
  }
}

class SpecifiedLengthExplicitBitsFixedParser(combinator: SpecifiedLengthCombinatorBase, e: ElementBase, nBits: Long)
  extends SpecifiedLengthParserBase(combinator, e) {

  val charset = e.knownEncodingCharset

  def parse(start: PState): PState = withParseErrorThrowing(start) {

    val in = start.inStream

    try {
      val nBytes = scala.math.ceil(nBits / 8.0).toLong
      val bytes = in.getBytes(start.bitPos, nBytes)
      val endBitPos = start.bitPos + nBits
      val postEState = parse(start, endBitPos, e)
      return postEState
    } catch {
      case ex: IndexOutOfBoundsException => {
        // Insufficient bits in field, but we need to still allow processing
        // to test for Nils
        val endBitPos = start.bitPos + 0
        val postEState = parse(start, endBitPos, e)
        return postEState
      }
      case u: UnsuppressableException => throw u
      case ex: Exception => { return PE(start, "SpecifiedLengthExplicitBitsFixedParser - Exception: \n%s", ex.getStackTraceString) }
    }
  }
}

class SpecifiedLengthExplicitBytesParser(combinator: SpecifiedLengthCombinatorBase, e: ElementBase)
  extends SpecifiedLengthParserBase(combinator, e) {

  val charset = e.knownEncodingCharset
  val expr = e.length

  def parse(start: PState): PState = withParseErrorThrowing(start) {

    val (pState, nBytes) = combinator.getLength(start)
    val in = pState.inStream

    try {
      val bytes = in.getBytes(pState.bitPos, nBytes)
      val endBitPos = pState.bitPos + (nBytes * 8)
      val postEState = parse(pState, endBitPos, e)
      return postEState
    } catch {
      case ex: IndexOutOfBoundsException => {
        // Insufficient bytes in field, but we need to still allow processing
        // to test for Nils
        val endBitPos = start.bitPos + 0
        val postEState = parse(start, endBitPos, e)
        return postEState
      }
      case u: UnsuppressableException => throw u
      case ex: Exception => { return PE(pState, "SpecifiedLengthExplicitBytesParser - Exception: \n%s", ex.getStackTraceString) }
    }
  }
}

class SpecifiedLengthExplicitBytesFixedParser(combinator: SpecifiedLengthCombinatorBase, e: ElementBase, nBytes: Long)
  extends SpecifiedLengthParserBase(combinator, e) {

  val charset = e.knownEncodingCharset

  def parse(start: PState): PState = withParseErrorThrowing(start) {

    val in = start.inStream

    try {
      // val bytes = in.getBytes(start.bitPos, nBytes)
      val endBitPos = start.bitPos + (nBytes * 8)
      val postEState = super.parse(start, endBitPos, e)
      return postEState
    } catch {
      case ex: IndexOutOfBoundsException => {
        // Insufficient bytes in field, but we need to still allow processing
        // to test for Nils
        val endBitPos = start.bitPos + 0
        val postEState = super.parse(start, endBitPos, e)
        return postEState
      }
      case u: UnsuppressableException => throw u
      case ex: Exception => { return PE(start, "SpecifiedLengthExplicitBytesFixedParser - Exception: \n%s", ex.getStackTraceString) }
    }
  }
}

class SpecifiedLengthExplicitCharactersFixedParser(combinator: SpecifiedLengthCombinatorBase, e: ElementBase, nChars: Long)
  extends SpecifiedLengthParserBase(combinator, e) {

  val charset = e.knownEncodingCharset

  def parse(start: PState): PState = withParseErrorThrowing(start) {

    val in = start.inStream
    val rdr = in.getCharReader(charset, start.bitPos)
    val d = new DelimParser(e.knownEncodingStringBitLengthFunction)
    val result = d.parseInputNCharacters(nChars, rdr, TextJustificationType.None, "")
    val endBitPos =
      result match {
        case _: DelimParseFailure => start.bitPos + 0 // no match == length is zero!
        case s: DelimParseSuccess => start.bitPos + s.numBits
      }
    val postEState = parse(start, endBitPos, e)
    postEState
  }
}

class SpecifiedLengthExplicitCharactersParser(combinator: SpecifiedLengthCombinatorBase, e: ElementBase)
  extends SpecifiedLengthParserBase(combinator, e) {

  val charset = e.knownEncodingCharset
  val expr = e.length

  def parse(start: PState): PState = withParseErrorThrowing(start) {

    val (pState, nChars) = combinator.getLength(start)
    val in = pState.inStream
    val rdr = in.getCharReader(charset, pState.bitPos)
    val d = new DelimParser(e.knownEncodingStringBitLengthFunction)
    val result = d.parseInputNCharacters(nChars, rdr, TextJustificationType.None, "")
    val endBitPos =
      result match {
        case _: DelimParseFailure => pState.bitPos + 0 // no match == length is zero!
        case s: DelimParseSuccess => pState.bitPos + s.numBits
      }
    val postEState = parse(pState, endBitPos, e)
    postEState
  }
}

