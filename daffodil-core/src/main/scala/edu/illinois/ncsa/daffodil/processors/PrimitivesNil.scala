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

import java.nio.ByteBuffer
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._

case class LiteralNilExplicitLengthInBytes(e: ElementBase)
  extends LiteralNilInBytesBase(e, "LiteralNilExplicit") {

  val expr = e.length
  val exprText = expr.prettyExpr

  final def computeLength(start: PState) = {
    val R(nBytesAsAny, newVMap) = expr.evaluate(start.parentElement, start.variableMap, start)
    val nBytes = nBytesAsAny.toString().toLong //nBytesAsAny.asInstanceOf[Long]
    (nBytes, newVMap)
  }

}

case class LiteralNilKnownLengthInBytes(e: ElementBase, lengthInBytes: Long)
  extends LiteralNilInBytesBase(e, "LiteralNilKnown") {

  final def computeLength(start: PState) = {
    (lengthInBytes, start.variableMap)
  }

}

abstract class LiteralNilInBytesBase(e: ElementBase, label: String)
  extends StaticText(e.nilValue, e, e, label, e.isNillable)
  with Padded {

  protected def computeLength(start: PState): (Long, VariableMap)

  // We are to assume that we can always read nBytes
  // a failure to read nBytes is a failure period.

  lazy val unparserDelim = Assert.notYetImplemented()
  lazy val d = new DFDLDelimParser(e.knownEncodingStringBitLengthFunction)

  override def parser = new PrimParser(this, e) {

    override def toBriefXML(depthLimit: Int = -1): String = {
      "<" + name + " nilValue='" + e.nilValue + "'/>"
    }

    val isEmptyAllowed = e.nilValue.contains("%ES;")
    val eName = e.toString()
    val nilValuesCooked = new ListOfStringValueAsLiteral(e.nilValue, e).cooked
    val charsetName = charset.name()

    def parse(start: PState): PState = {
      //      withLoggingLevel(LogLevel.Debug) 
      {

        // TODO: What if someone passes in nBytes = 0 for Explicit length, is this legal?

        val (nBytes: Long, newVMap: VariableMap) = computeLength(start)
        val postEvalState = start.withVariables(newVMap)
        log(LogLevel.Debug, "Explicit length %s", nBytes)

        //val postEvalState = start //start.withVariables(vars)

        log(LogLevel.Debug, "%s - Looking for: %s Count: %s", eName, nilValuesCooked, nilValuesCooked.length)
        val in = postEvalState.inStream

        val bytePos = (postEvalState.bitPos >> 3).toInt
        log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, postEvalState.bitPos)
        log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

        // some encodings aren't whole bytes
        // if (postEvalState.bitPos % 8 != 0) { return PE(postEvalState, "LiteralNilPattern - not byte aligned.") }

        val decoder = charset.newDecoder()

        try {
          val reader = in.getCharReader(charset, postEvalState.bitPos)
          val bytes = in.getBytes(postEvalState.bitPos, nBytes.toInt)
          val cb = decoder.decode(ByteBuffer.wrap(bytes))
          val result = cb.toString
          val trimmedResult = trimByJustification(result)
          val endBitPos = postEvalState.bitPos + (nBytes.toInt * 8)
          val endCharPos = if (postEvalState.charPos == -1) result.length() else postEvalState.charPos + result.length()

          // We have a field, is it empty?
          val isFieldEmpty = trimmedResult.length == 0 //result.length() == 0

          if (isFieldEmpty && isEmptyAllowed) {
            // Valid!
            postEvalState.parentElement.makeNil()
            return postEvalState // Empty, no need to advance
          } else if (isFieldEmpty && !isEmptyAllowed) {
            // Fail!
            return PE(postEvalState, "%s - Empty field found but not allowed!", eName)
          } else if (d.isFieldDfdlLiteral(trimmedResult, nilValuesCooked.toSet)) {
            // Contains a nilValue, Success!
            postEvalState.parentElement.makeNil()

            log(LogLevel.Debug, "%s - Found %s", eName, trimmedResult)
            log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPos >> 3))
            log(LogLevel.Debug, "%s - Ended at bit position ", eName, endBitPos)

            return postEvalState.withPos(endBitPos, endCharPos, One(reader)) // Need to advance past found nilValue
          } else {
            // Fail!
            return PE(postEvalState, "%s - Does not contain a nil literal!", eName)
          }
        } catch {
          case e: IndexOutOfBoundsException => {
            // In this case, we failed to get the bytes
            if (isEmptyAllowed) {
              // Valid!
              postEvalState.parentElement.makeNil()
              return postEvalState // Empty, no need to advance
            } else {
              return PE(postEvalState, "%s - Insufficient Bytes in field; required %s", name, nBytes)
            }
          }
          case u: UnsuppressableException => throw u
          case e: Exception => { return PE(postEvalState, "%s - Exception: \n%s", name, e.getMessage()) }
        }
      }
    }

  }

  override def unparser: Unparser = new Unparser(e) {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

case class LiteralNilExplicitLengthInChars(e: ElementBase)
  extends StaticText(e.nilValue, e, e, "LiteralNilExplicit", e.isNillable)
  with Padded {
  // We are to assume that we can always read nChars
  // a failure to read nChars is a failure period.

  // TODO: LiteralNilExplicitLengthInChars really is a variation of LiteralNilPattern
  lazy val unparserDelim = Assert.notYetImplemented()
  lazy val d = new DFDLDelimParser(e.knownEncodingStringBitLengthFunction)

  override def parser = new PrimParser(this, e) {

    override def toBriefXML(depthLimit: Int = -1): String = {
      "<" + name + " nilValue='" + e.nilValue + "'/>"
    }

    val isEmptyAllowed = e.nilValue.contains("%ES;")
    val eName = e.toString()
    val nilValuesCooked = new ListOfStringValueAsLiteral(e.nilValue, e).cooked
    val charsetName = charset.name()
    val expr = e.length
    val exprText = expr.prettyExpr

    def parse(start: PState): PState = {
      // withLoggingLevel(LogLevel.Info) 
      {

        //val postEvalState = start //start.withVariables(vars)

        val R(nCharsAsAny, newVMap) = expr.evaluate(start.parentElement, start.variableMap, start)
        val nChars = nCharsAsAny.asInstanceOf[String] //nBytesAsAny.asInstanceOf[Long]
        val postEvalState = start.withVariables(newVMap)
        log(LogLevel.Debug, "Explicit length %s", nChars)

        val pattern = "(?s)^.{%s}".format(nChars)

        log(LogLevel.Debug, "%s - Looking for: %s Count: %s", eName, nilValuesCooked, nilValuesCooked.length)

        val bytePos = (postEvalState.bitPos >> 3).toInt
        log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, postEvalState.bitPos)
        log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

        // Don't check this here. This can vary by encoding.
        //if (postEvalState.bitPos % 8 != 0) { return PE(start, "LiteralNilPattern - not byte aligned.") }

        log(LogLevel.Debug, "Retrieving reader state.")
        val reader = getReader(charset, start.bitPos, start)

        if (nChars == 0 && isEmptyAllowed) {
          log(LogLevel.Debug, "%s - explicit length of 0 and %ES; found as nilValue.", eName)
          postEvalState.parentElement.makeNil()
          return postEvalState // Empty, no need to advance
        }

        val result = d.parseInputPatterned(pattern, reader, postEvalState)

        result match {
          case _: DelimParseFailure =>
            return PE(postEvalState, "%s - %s - Parse failed.", this.toString(), eName)
          case s: DelimParseSuccess => {
            // We have a field, is it empty?
            val field = trimByJustification(s.field)
            val isFieldEmpty = field.length() == 0

            if (isFieldEmpty && isEmptyAllowed) {
              // Valid!
              start.parentElement.makeNil()
              return postEvalState // Empty, no need to advance
            } else if (isFieldEmpty && !isEmptyAllowed) {
              // Fail!
              return PE(postEvalState, "%s - Empty field found but not allowed!", eName)
            } else if (d.isFieldDfdlLiteral(field, nilValuesCooked.toSet)) {
              // Contains a nilValue, Success!
              start.parentElement.makeNil()

              val numBits = s.numBits //e.knownEncodingStringBitLength(result.field)
              val endCharPos =
                if (postEvalState.charPos == -1) s.field.length
                else postEvalState.charPos + s.field.length
              val endBitPos = numBits + start.bitPos

              log(LogLevel.Debug, "%s - Found %s", eName, s.field)
              log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPos >> 3))
              log(LogLevel.Debug, "%s - Ended at bit position ", eName, endBitPos)

              return postEvalState.withPos(endBitPos, endCharPos, One(s.next)) // Need to advance past found nilValue
            } else {
              // Fail!
              return PE(postEvalState, "%s - Does not contain a nil literal!", eName)
            }
          }
        }
      }
    }
  }

  override def unparser: Unparser = new Unparser(e) {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }

}

case class LiteralNilExplicit(e: ElementBase, nUnits: Long)
  extends StaticText(e.nilValue, e, e, "LiteralNilExplicit", e.isNillable)
  with Padded {
  lazy val unparserDelim = Assert.notYetImplemented()
  //val stParser = super.parser

  lazy val d = new DFDLDelimParser(e.knownEncodingStringBitLengthFunction)

  override def parser = new PrimParser(this, e) {

    override def toBriefXML(depthLimit: Int = -1): String = {
      "<" + name + " nilValue='" + e.nilValue + "'/>"
    }

    val pattern = e.lengthPattern

    val isEmptyAllowed = e.nilValue.contains("%ES;")
    val eName = e.toString()
    val nilValuesCooked = new ListOfStringValueAsLiteral(e.nilValue, e).cooked
    val charsetName = charset.name()

    def parse(start: PState): PState = {
      // withLoggingLevel(LogLevel.Info) 
      {

        val postEvalState = start //start.withVariables(vars)

        log(LogLevel.Debug, "%s - Looking for: %s Count: %s", eName, nilValuesCooked, nilValuesCooked.length)

        val bytePos = (postEvalState.bitPos >> 3).toInt
        log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, postEvalState.bitPos)
        log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

        if (postEvalState.bitPos % 8 != 0) { return PE(start, "LiteralNilPattern - not byte aligned.") }

        log(LogLevel.Debug, "Retrieving reader state.")
        val reader = getReader(charset, start.bitPos, start)

        //        val byteReader = in.byteReader.atPos(bytePos)
        //        val reader = byteReader.charReader(decoder.charset().name())

        val result = d.parseInputPatterned(pattern, reader, start)

        result match {
          case _: DelimParseFailure =>
            return PE(postEvalState, "%s - %s - Parse failed.", this.toString(), eName)
          case s: DelimParseSuccess => {
            // We have a field, is it empty?
            val field = trimByJustification(s.field)
            val isFieldEmpty = field.length() == 0

            if (isFieldEmpty && isEmptyAllowed) {
              // Valid!
              start.parentElement.makeNil()
              return postEvalState // Empty, no need to advance
            } else if (isFieldEmpty && !isEmptyAllowed) {
              // Fail!
              return PE(postEvalState, "%s - Empty field found but not allowed!", eName)
            } else if (d.isFieldDfdlLiteral(field, nilValuesCooked.toSet)) {
              // Contains a nilValue, Success!
              start.parentElement.makeNil()

              val numBits = s.numBits //e.knownEncodingStringBitLength(result.field)
              //val endCharPos = start.charPos + result.field.length()
              val endCharPos =
                if (postEvalState.charPos == -1) s.field.length
                else postEvalState.charPos + s.field.length
              val endBitPos = numBits + start.bitPos

              log(LogLevel.Debug, "%s - Found %s", eName, s.field)
              log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPos >> 3))
              log(LogLevel.Debug, "%s - Ended at bit position ", eName, endBitPos)

              //return postEvalState.withPos(endBitPos, endCharPos) // Need to advance past found nilValue
              return postEvalState.withPos(endBitPos, endCharPos, One(s.next)) // Need to advance past found nilValue
            } else {
              // Fail!
              return PE(postEvalState, "%s - Does not contain a nil literal!", eName)
            }
          }
        }
      }
    }
  }

  override def unparser: Unparser = new Unparser(e) {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

case class LiteralNilPattern(e: ElementBase)
  extends StaticText(e.nilValue, e, e, "LiteralNilPattern", e.isNillable)
  with Padded {
  lazy val unparserDelim = Assert.notYetImplemented()
  //val stParser = super.parser
  lazy val d = new DFDLDelimParser(e.knownEncodingStringBitLengthFunction)

  override def parser = new PrimParser(this, e) {

    override def toBriefXML(depthLimit: Int = -1): String = {
      "<" + name + " nilValue='" + e.nilValue + "'/>"
    }

    val pattern = e.lengthPattern

    val isEmptyAllowed = e.nilValue.contains("%ES;")
    val eName = e.toString()
    val nilValuesCooked = new ListOfStringValueAsLiteral(e.nilValue, e).cooked
    val charsetName = charset.name()

    def parse(start: PState): PState = {
      // withLoggingLevel(LogLevel.Info) 
      {

        val postEvalState = start //start.withVariables(vars)

        log(LogLevel.Debug, "%s - Looking for: %s Count: %s", eName, nilValuesCooked, nilValuesCooked.length)

        val bytePos = (postEvalState.bitPos >> 3).toInt
        log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, postEvalState.bitPos)
        log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

        if (postEvalState.bitPos % 8 != 0) { return PE(start, "LiteralNilPattern - not byte aligned.") }

        log(LogLevel.Debug, "Retrieving reader state.")
        val reader = getReader(charset, start.bitPos, start)

        val result = d.parseInputPatterned(pattern, reader, start)

        result match {
          case _: DelimParseFailure =>
            return PE(postEvalState, "%s - %s - Parse failed.", this.toString(), eName)
          case s: DelimParseSuccess => {
            // We have a field, is it empty?
            val field = trimByJustification(s.field)
            val isFieldEmpty = field.length() == 0

            if (isFieldEmpty && isEmptyAllowed) {
              // Valid!
              start.parentElement.makeNil()
              return postEvalState // Empty, no need to advance
            } else if (isFieldEmpty && !isEmptyAllowed) {
              // Fail!
              return PE(postEvalState, "%s - Empty field found but not allowed!", eName)
            } else if (d.isFieldDfdlLiteral(field, nilValuesCooked.toSet)) {
              // Contains a nilValue, Success!
              start.parentElement.makeNil()

              val numBits = s.numBits //e.knownEncodingStringBitLength(result.field)

              val endCharPos =
                if (postEvalState.charPos == -1) s.field.length
                else postEvalState.charPos + s.field.length
              val endBitPos = numBits + start.bitPos

              log(LogLevel.Debug, "%s - Found %s", eName, s.field)
              log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPos >> 3))
              log(LogLevel.Debug, "%s - Ended at bit position ", eName, endBitPos)

              return postEvalState.withPos(endBitPos, endCharPos, One(s.next)) // Need to advance past found nilValue
            } else {
              // Fail!
              return PE(postEvalState, "%s - Does not contain a nil literal!", eName)
            }
          }
        }
      }
    }
  }

  override def unparser: Unparser = new Unparser(e) {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

case class LogicalNilValue(e: ElementBase) extends Primitive(e, e.isNillable)
