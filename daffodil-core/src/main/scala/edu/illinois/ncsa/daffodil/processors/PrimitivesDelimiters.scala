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

import java.math.BigInteger
import java.text.{ ParseException, ParsePosition }
import java.util.regex.Pattern
import java.nio.{ CharBuffer, ByteBuffer }
import java.nio.charset.Charset
import java.nio.charset.MalformedInputException
import scala.collection.mutable.Queue
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.{ YesNo, LengthKind, ByteOrder, LengthUnits }
import edu.illinois.ncsa.daffodil.util.{ Debug, LogLevel, Logging, Info }
import edu.illinois.ncsa.daffodil.util.Misc.bytes2Hex
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import com.ibm.icu.text.{ NumberFormat, DecimalFormat }
import edu.illinois.ncsa.daffodil.grammar.Terminal
import scala.util.parsing.input.{ Reader }
import java.sql.Timestamp
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextTrimKind
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextStringJustification
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextNumberJustification
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextCalendarJustification
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextBooleanJustification
import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }
import scala.util.parsing.combinator.RegexParsers

abstract class StaticDelimiter(kindString: String, delim: String, e: Term, guard: Boolean = true)
  extends StaticText(delim, e, kindString, guard)

abstract class StaticText(delim: String, e: Term, kindString: String, guard: Boolean = true)
  extends Text(e, guard) //extends DelimParserBase(e, guard)
  with WithParseErrorThrowing with TextReader {

  val charset = e.knownEncodingCharset
  val term = e.asInstanceOf[Term]

  val staticTexts = delim.split("\\s").toList
  val staticTextsCooked: Queue[String] = new Queue

  staticTexts.foreach(x => staticTextsCooked.enqueue(EntityReplacer.replaceAll(x)))

  val delimsRaw = e.allTerminatingMarkup.map {
    case (delimValue, elemName, elemPath) => (delimValue.constantAsString, elemName, elemPath)
  }
  val delimsCooked1 = delimsRaw.map {
    case (delimValue, elemName, elemPath) => {
      (new ListOfStringValueAsLiteral(delimValue.toString, e).cooked, elemName, elemPath)
    }
  }
  val delimsCooked = delimsCooked1.map { case (delimValue, _, _) => delimValue }.flatten

  // Here we expect that remoteDelims shall be defined as those delimiters who are not
  // also defined locally.  That is to say that local should win over remote.
  val remoteDelims = delimsCooked.toSet.diff(staticTextsCooked.toSet)

  // here we define the parsers so that they are pre-compiled/generated
  val delimParser = new DFDLDelimParserStatic(e.knownEncodingStringBitLengthFunction)
  val (pInputDelimiterParser, pIsLocalDelimParser, remoteDelimRegex) =
    delimParser.generateInputDelimiterParsers(staticTextsCooked.toSet, remoteDelims)

  def parseMethod(input: Reader[Char]): DelimParseResult = {
    val result: DelimParseResult = delimParser.parseInputDelimiter(pInputDelimiterParser, pIsLocalDelimParser, input)
    result
  }

  //e.schemaDefinitionWarningUnless(e.ignoreCase == YesNo.No, "Property ignoreCase='yes' not supported.")
  Assert.invariant(delim != "") // shouldn't be here at all in this case.

  def parser: DaffodilParser = new PrimParser(this, e) {

    override def toBriefXML(depthLimit: Int = -1) = {
      "<" + kindString + ">" + delim + " " + delimsRaw + "</" + kindString + ">"
    }

    override def toString = kindString + "('" + delim + "')" //  with terminating markup: " + term.prettyTerminatingMarkup + ")"

    // This has to stay here, moving it outside of PrimParser causes it
    // to not be defined. Why?
    e.schemaDefinitionWarningUnless(e.ignoreCase == YesNo.No, "Property ignoreCase='yes' not supported.")

    val eName = e.toString()

    def parse(start: PState): PState = withParseErrorThrowing(start) {
      // withLoggingLevel(LogLevel.Debug) 
      {

        log(LogLevel.Debug, "%s - Parsing delimiter at byte position: %s", eName, (start.bitPos >> 3))
        log(LogLevel.Debug, "%s - Parsing delimiter at bit position: %s", eName, start.bitPos)

        log(LogLevel.Debug, "%s - Looking for local(%s) not remote (%s).", eName, staticTextsCooked.toSet, remoteDelims)

        val bytePos = (start.bitPos >> 3).toInt

        log(LogLevel.Debug, "Retrieving reader state.")
        val reader = getReader(charset, start.bitPos, start)

        // Well they may not be delimiters, but the logic is the same as for a 
        // set of static delimiters.
        val result = parseMethod(reader)

        log(LogLevel.Debug, "%s - %s - DelimParseResultult: %s", this.toString(), eName, result)

        result match {
          case _: DelimParseFailure => {
            log(LogLevel.Debug, "%s - %s: Delimiter not found!", this.toString(), eName)
            return PE(start, "%s - %s: Delimiter not found!", this.toString(), eName)
          }
          case s: DelimParseSuccess if (s.delimiterLoc == DelimiterLocation.Remote) => {
            val (remoteDelimValue, remoteElemName, remoteElemPath) =
              getMatchedDelimiterInfo(remoteDelimRegex, s.delimiter, delimsCooked1)

            log(LogLevel.Debug, "%s - %s: Found delimiter (%s) for %s when looking for %s(%s) for %s %s",
              this.toString(), eName, remoteDelimValue, remoteElemPath, kindString, staticTexts.mkString(" "), e.path, positionalInfo)
            return PE(start, "%s - %s: Found delimiter (%s) for %s when looking for %s(%s) for %s %s",
              this.toString(), eName, remoteDelimValue, remoteElemPath, kindString, staticTexts.mkString(" "), e.path, positionalInfo)
          }
          case s: DelimParseSuccess =>
            {
              val numBits = e.knownEncodingStringBitLengthFunction(s.delimiter)
              val endCharPos = if (start.charPos == -1) s.delimiter.length else start.charPos + s.delimiter.length()
              val endBitPosDelim = numBits + start.bitPos

              log(LogLevel.Debug, "%s - Found %s", eName, s.delimiter)
              log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPosDelim >> 3))
              log(LogLevel.Debug, "%s - Ended at bit position %s", eName, endBitPosDelim)

              return start.withPos(endBitPosDelim, endCharPos, Some(s.next))
            }
            start
        }
      }
    }
  }

  def unparser: Unparser = new Unparser(e) {
    val t = e.asInstanceOf[Term]
    override def toString = "StaticText('" + delim + "' with terminating markup: " + t.prettyTerminatingMarkup + ")"
    // setLoggingLevel(LogLevel.Info)
    e.schemaDefinitionWarningUnless(e.ignoreCase == YesNo.No, "Property ignoreCase='yes' is not supported.")
    Assert.invariant(delim != "") //shouldn't be here at all in this case

    def unparse(start: UState): UState = {
      val encoder = e.knownEncodingCharset.newEncoder()
      start.outStream.setEncoder(encoder)
      start.outStream.fillCharBuffer(unparserDelim)
      log(LogLevel.Debug, "Unparsed: " + start.outStream.getData)
      start
    }
  }

  def unparserDelim: String
}

abstract class Text(e: Term, guard: Boolean) extends DelimParserBase(e, guard) {
  lazy val oes = {
    val oes = e.optionEscapeScheme
    oes.foreach { es =>
      e.schemaDefinitionUnless(es.isKnownEscapeCharacter != Some(false), "Runtime expressions for escapeCharacters are not supported.")
      e.schemaDefinitionUnless(es.isKnownEscapeEscapeCharacter != Some(false), "Runtime expressions for escapeEscapeCharacters are not supported.")
    }
    oes
  }
  lazy val esObj = EscapeScheme.getEscapeScheme(oes, e)
  val eName = e.toString()

  val positionalInfo = {
    if (e.isDirectChildOfSequence) {
      e.nearestEnclosingSequence match {
        case Some(es) => {
          val pos = e.positionInNearestEnclosingSequence - 1
          if (es.hasPrefixSep) {
            if (e.hasPriorRequiredSiblings) {
              val prior: Term = es.groupMembers(pos - 1)
              "after " + prior.prettyName + " and before " + eName
            } else "before " + eName
          } else if (es.hasInfixSep)
            if (e.hasPriorRequiredSiblings && e.hasLaterRequiredSiblings) {
              val prior: Term = es.groupMembers(pos - 1)

              "after " + prior.prettyName + " and before " + eName
            } else if (e.hasPriorRequiredSiblings) {
              val prior: Term = es.groupMembers(pos - 1)
              "after " + prior.prettyName + " and before " + eName
            } else if (e.hasLaterRequiredSiblings) {
              val later: Term = es.groupMembers(pos + 1)
              "before " + later.prettyName
            } else { "" }
          else if (es.hasPostfixSep)
            if (e.hasPriorRequiredSiblings && e.hasLaterRequiredSiblings) {
              val later: Term = es.groupMembers(pos + 1)

              "after " + eName + " and before " + later.prettyName
            } else if (e.hasPriorRequiredSiblings) {
              val prior: Term = es.groupMembers(pos - 1)
              "after " + prior.prettyName + " and before " + eName
            } else if (e.hasLaterRequiredSiblings) {
              val later: Term = es.groupMembers(pos + 1)
              "before " + later.prettyName
            } else { "" }
          else
            ""
        }
        case None => ""
      }
    }
  }

  def getMatchedDelimiterInfo(remoteDelimRegex: Set[(String, String)], foundDelimiter: String,
    delimiters: List[(List[String], String, String)]) = {
    val matchedDelim = remoteDelimRegex.find {
      case (delimRegex, _) => {
        foundDelimiter.matches("(?s)^(" + delimRegex + ")$")
      }
    } match {
      case Some((_, theValue)) => theValue
      case None => Assert.impossibleCase()
    }

    val (remoteDelimValue, remoteElemName, remoteElemPath, _) =
      {
        val findResult = delimiters.map {
          case (delimValueList, elemName, elemPath) => {
            delimValueList.find(delim => delim == matchedDelim) match {
              case Some(d) => (d, elemName, elemPath, true)
              case None => (delimValueList.mkString(","), elemName, elemPath, false)
            }
          }
        }.toSet.filter { x => x._4 == true }

        if (findResult.size == 0) Assert.impossibleCase()
        findResult.head
      }
    (remoteDelimValue, remoteElemName, remoteElemPath)
  }

}

abstract class DynamicText(delimExpr: CompiledExpression, e: Term, kindString: String, guard: Boolean = true)
  extends Text(e, guard)
  with WithParseErrorThrowing with TextReader {

  val charset = e.knownEncodingCharset
  val term = e.asInstanceOf[Term]

  // If there are any static delimiters, pre-process them here
  lazy val staticDelimsRaw =
    e.allTerminatingMarkup.filter {
      case (delimValue, _, _) => delimValue.isConstant
    }.map {
      case (delimValue, eName, ePath) => (delimValue.constantAsString, eName, ePath)
    }
  lazy val staticDelimsCooked1 = staticDelimsRaw.map {
    case (delimValue, elemName, elemPath) => { (new ListOfStringValueAsLiteral(delimValue.toString, e).cooked, elemName, elemPath) }
  }
  lazy val staticDelimsCooked = staticDelimsCooked1.map { case (delimValue, _, _) => delimValue }.flatten
  lazy val (staticDelimsParsers, staticDelimsRegex) = dp.generateDelimiter(staticDelimsCooked.toSet)

  def parseMethod(pInputDelimiterParser: dp.Parser[String],
    pIsLocalDelimParser: dp.Parser[String],
    input: Reader[Char]): DelimParseResult = {
    val result: DelimParseResult = dp.parseInputDelimiter(pInputDelimiterParser, pIsLocalDelimParser, input)
    result
  }

  def parser: DaffodilParser = new PrimParser(this, e) {

    override def toBriefXML(depthLimit: Int = -1) = {
      "<" + kindString + ">" + delimExpr + " " + delimExpr + "</" + kindString + ">"
    }

    e.schemaDefinitionWarningUnless(e.ignoreCase == YesNo.No, "Property ignoreCase='yes' not supported.")

    Assert.invariant(delimExpr.toString != "") // shouldn't be here at all in this case.
    override def toString = kindString + "('" + delimExpr + "')" //  with terminating markup: " + term.prettyTerminatingMarkup + ")"

    lazy val tm = e.allTerminatingMarkup
    val eName = e.toString()

    def parse(start: PState): PState = withParseErrorThrowing(start) {
      // withLoggingLevel(LogLevel.Debug) 
      {

        // We must feed variable context out of one evaluation and into the next.
        // So that the resulting variable map has the updated status of all evaluated variables.
        var vars = start.variableMap

        val dynamicDelimsRaw = e.allTerminatingMarkup.filter { case (delimValue, elemName, elemPath) => !delimValue.isConstant }.map {
          case (delimValue, elemName, elemPath) =>
            {
              val R(res, newVMap) = delimValue.evaluate(start.parentElement, vars, start)
              vars = newVMap
              (res, elemName, elemPath)
            }
        }
        // Dynamic delimiters can only be evaluated at runtime
        val dynamicDelimsCooked1 = dynamicDelimsRaw.map {
          case (delimValue, elemValue, elemPath) => { (new ListOfStringValueAsLiteral(delimValue.toString, e).cooked, elemValue, elemPath) }
        }
        val dynamicDelimsCooked = dynamicDelimsCooked1.map { case (delimValue, _, _) => delimValue }.flatten
        val delimsCooked = dynamicDelimsCooked.union(staticDelimsCooked)
        val (dynamicDelimsParsers, dynamicDelimsRegex) = dp.generateDelimiter(dynamicDelimsCooked.toSet)

        val localDelimsRaw = {
          val R(res, newVMap) = delimExpr.evaluate(start.parentElement, vars, start)
          vars = newVMap
          res
        }
        val localDelimsCooked1 = new ListOfStringValueAsLiteral(localDelimsRaw.toString(), e).cooked
        val localDelimsCooked = localDelimsCooked1
        val (localDelimsParser, localDelimsRegex) = dp.generateDelimiter(localDelimsCooked.toSet)

        val pIsLocalDelimParser = dp.generateIsLocalDelimParser(localDelimsRegex)

        val postEvalState = start.withVariables(vars)

        log(LogLevel.Debug, "%s - Parsing delimiter at byte position: %s", eName, (postEvalState.bitPos >> 3))
        log(LogLevel.Debug, "%s - Parsing delimiter at bit position: %s", eName, postEvalState.bitPos)

        log(LogLevel.Debug, "%s - Looking for local(%s) not remote (%s).", eName, localDelimsCooked.toSet, delimsCooked.toSet)

        val bytePos = (postEvalState.bitPos >> 3).toInt

        log(LogLevel.Debug, "Retrieving reader state.")
        val reader = getReader(charset, start.bitPos, postEvalState)

        val remoteDelims: Array[dp.Parser[String]] = staticDelimsParsers.union(dynamicDelimsParsers)
        val remoteDelimRegex = dp.getDelimsRegex(staticDelimsCooked.union(dynamicDelimsCooked).toSet)
        val pInputDelimiterParser = dp.generateInputDelimiterParser(localDelimsParser, remoteDelims)

        val result = parseMethod(pInputDelimiterParser, pIsLocalDelimParser, reader)

        log(LogLevel.Debug, "%s - %s - DelimParseResultult: %s", this.toString(), eName, result)

        result match {
          case _: DelimParseFailure => {
            log(LogLevel.Debug, "%s - %s: Delimiter not found!", this.toString(), eName)
            return PE(start, "%s - %s: Delimiter not found!", this.toString(), eName)
          }
          case s: DelimParseSuccess if (s.delimiterLoc == DelimiterLocation.Remote) => {
            val (remoteDelimValue, remoteElemName, remoteElemPath) =
              getMatchedDelimiterInfo(remoteDelimRegex, s.delimiter, dynamicDelimsCooked1)

            log(LogLevel.Debug, "%s - %s: Found delimiter (%s) for %s when looking for %s(%s) for %s",
              this.toString(), eName, remoteDelimValue, remoteElemPath, kindString, localDelimsCooked.mkString(" "), e.path)
            return PE(start, "%s - %s: Found delimiter (%s) for %s when looking for %s(%s) for %s",
              this.toString(), eName, remoteDelimValue, remoteElemPath, kindString, localDelimsCooked.mkString(" "), e.path)
          }
          case s: DelimParseSuccess =>
            {
              val numBits = e.knownEncodingStringBitLengthFunction(s.delimiter)
              val endCharPos = if (postEvalState.charPos == -1) s.delimiter.length else postEvalState.charPos + s.delimiter.length()
              val endBitPosDelim = numBits + postEvalState.bitPos

              log(LogLevel.Debug, "%s - Found %s", eName, s.delimiter)
              log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPosDelim >> 3))
              log(LogLevel.Debug, "%s - Ended at bit position %s", eName, endBitPosDelim)

              return postEvalState.withPos(endBitPosDelim, endCharPos, Some(s.next))
            }
            postEvalState
        }
      }
    }
  }

  /*
  def unparser: Unparser = new Unparser(e) {
    val t = e.asInstanceOf[Term]
    override def toString = "StaticText('" + delimExpr + "' with terminating markup: " + t.prettyTerminatingMarkup + ")"
    // setLoggingLevel(LogLevel.Info)
    e.schemaDefinitionWarningUnless(e.ignoreCase == YesNo.No, "Property ignoreCase='yes' is not supported.")
    Assert.invariant(delimExpr != "") //shouldn't be here at all in this case

    def unparse(start: UState): UState = {
      // We really want to do something similar to the below to evaluate the expression
      // for a delimiter.
      //      val localDelimsRaw = {
      //        val R(res, newVMap) = delimExpr.evaluate(start.parentElement, vars, start)
      //        vars = newVMap
      //        res
      //      }
      //      val localDelimsCooked1 = new ListOfStringValueAsLiteral(localDelimsRaw.toString(), e).cooked
      //      val localDelimsCooked = localDelimsCooked1
      val encoder = e.knownEncodingCharset.newEncoder()
      start.outStream.setEncoder(encoder)

      // TODO: This is not correct, we need to be able to evaluate delimExpr and select a
      // delimiter to use here.
      start.outStream.fillCharBuffer(delimExpr.toString()) //start.outStream.fillCharBuffer(unparserDelim)
      log(LogLevel.Debug, "Unparsed: " + start.outStream.getData))
      start
    }
  } */
  def unparser: Unparser = DummyUnparser

  //def unparserDelim: String
}

abstract class DynamicDelimiter(kindString: String, delimExpr: CompiledExpression, e: Term, guard: Boolean = true)
  extends DynamicText(delimExpr, e, kindString, guard)

//case class StaticInitiator(e: Term) extends StaticDelimiter(e.initiator.constantAsString, e)
case class StaticInitiator(e: Term) extends StaticDelimiter("Init", e.initiator.constantAsString, e) {
  Assert.invariant(e.hasInitiator)
  lazy val unparserDelim = e.initiator.constantAsString.split("""\s""").head
}
//case class StaticTerminator(e : Term) extends StaticDelimiter(e.terminator.constantAsString, e)
case class StaticTerminator(e: Term) extends StaticDelimiter("Term", e.terminator.constantAsString, e) {
  Assert.invariant(e.hasTerminator)
  lazy val unparserDelim = e.terminator.constantAsString.split("""\s""").head
}
case class DynamicInitiator(e: Term) extends DynamicDelimiter("Init", e.initiator, e)
case class DynamicTerminator(e: Term) extends DynamicDelimiter("Term", e.terminator, e)

case class StaticSeparator(s: Sequence, t: Term) extends StaticDelimiter("Sep", s.separator.constantAsString, t) {
  Assert.invariant(s.hasSeparator)
  lazy val unparserDelim = s.separator.constantAsString.split("""\s""").head
}
case class DynamicSeparator(s: Sequence, t: Term) extends DynamicDelimiter("Sep", s.separator, t)

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
  extends StaticText(e.nilValue, e, label, e.isNillable)
  with Padded {

  protected def computeLength(start: PState): (Long, VariableMap)

  // We are to assume that we can always read nBytes
  // a failure to read nBytes is a failure period.

  lazy val unparserDelim = Assert.notYetImplemented()

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
        val d = new DelimParser(e.knownEncodingStringBitLengthFunction)
        try {
          val reader = in.getCharReader(charset, postEvalState.bitPos)
          val bytes = in.getBytes(postEvalState.bitPos, nBytes.toInt)
          val cb = decoder.decode(ByteBuffer.wrap(bytes))
          val result = cb.toString
          val trimmedResult = d.removePadding(result, justificationTrim, padChar)
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

            return postEvalState.withPos(endBitPos, endCharPos, Some(reader)) // Need to advance past found nilValue
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
  extends StaticText(e.nilValue, e, "LiteralNilExplicit", e.isNillable)
  with Padded {
  // We are to assume that we can always read nChars
  // a failure to read nChars is a failure period.

  // TODO: LiteralNilExplicitLengthInChars really is a variation of LiteralNilPattern
  lazy val unparserDelim = Assert.notYetImplemented()

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

        val d = new DelimParser(e.knownEncodingStringBitLengthFunction)

        val result = d.parseInputPatterned(pattern, reader)

        result match {
          case _: DelimParseFailure =>
            return PE(postEvalState, "%s - %s - Parse failed.", this.toString(), eName)
          case s: DelimParseSuccess => {
            // We have a field, is it empty?
            val field = d.removePadding(s.field, justificationTrim, padChar)
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

              return postEvalState.withPos(endBitPos, endCharPos, Some(s.next)) // Need to advance past found nilValue
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
  extends StaticText(e.nilValue, e, "LiteralNilExplicit", e.isNillable)
  with Padded {
  lazy val unparserDelim = Assert.notYetImplemented()
  //val stParser = super.parser

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

        val d = new DelimParser(e.knownEncodingStringBitLengthFunction)

        val result = d.parseInputPatterned(pattern, reader)

        result match {
          case _: DelimParseFailure =>
            return PE(postEvalState, "%s - %s - Parse failed.", this.toString(), eName)
          case s: DelimParseSuccess => {
            // We have a field, is it empty?
            val field = d.removePadding(s.field, justificationTrim, padChar)
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
              return postEvalState.withPos(endBitPos, endCharPos, Some(s.next)) // Need to advance past found nilValue
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
  extends StaticText(e.nilValue, e, "LiteralNilPattern", e.isNillable)
  with Padded {
  lazy val unparserDelim = Assert.notYetImplemented()
  //val stParser = super.parser

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

        val d = new DelimParser(e.knownEncodingStringBitLengthFunction)

        val result = d.parseInputPatterned(pattern, reader)

        result match {
          case _: DelimParseFailure =>
            return PE(postEvalState, "%s - %s - Parse failed.", this.toString(), eName)
          case s: DelimParseSuccess => {
            // We have a field, is it empty?
            val field = d.removePadding(s.field, justificationTrim, padChar)
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

              return postEvalState.withPos(endBitPos, endCharPos, Some(s.next)) // Need to advance past found nilValue
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

abstract class LiteralNilDelimitedEndOfData(eb: ElementBase)
  extends StringDelimited(eb) {
  val nilValuesCooked = new ListOfStringValueAsLiteral(eb.nilValue, eb).cooked
  val isEmptyAllowed = eb.nilValue.contains("%ES;") // TODO: move outside parser

  override def processResult(result: DelimParseResult, state: PState): PState = {
    result match {
      case f: DelimParseFailure =>
        return parser.PE(state, "%s - %s - Parse failed.", this.toString(), eName)
      case s: DelimParseSuccess => {
        // We have a field, is it empty?
        //val field = dp.removePadding(s.field, justificationTrim, padChar)
        val field = s.field
        val isFieldEmpty = field.length() == 0 // Note: field has been stripped of padChars

        if (isFieldEmpty && !isEmptyAllowed) {
          // Fail!
          return parser.PE(state, "%s - Empty field found but not allowed!", eName)
        } else if ((isFieldEmpty && isEmptyAllowed) || // Empty, but must advance past padChars if there were any. 
          dp.isFieldDfdlLiteral(field, nilValuesCooked.toSet)) { // Not empty, but matches.
          // Contains a nilValue, Success!
          state.parentElement.makeNil()

          val numBits = s.numBits
          //val endCharPos = start.charPos + result.field.length()
          val endCharPos = if (state.charPos == -1) s.numCharsRead else state.charPos + s.numCharsRead
          val endBitPos = numBits + state.bitPos

          log(LogLevel.Debug, "%s - Found %s", eName, s.field)
          log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPos >> 3))
          log(LogLevel.Debug, "%s - Ended at bit position ", eName, endBitPos)

          //return postEvalState.withPos(endBitPos, endCharPos) // Need to advance past found nilValue
          return state.withPos(endBitPos, endCharPos, Some(s.next)) // Need to advance past found nilValue
        } else {
          // Fail!
          return parser.PE(state, "%s - Does not contain a nil literal!", eName)
        }
      }
    }
  }
}

case class LiteralNilDelimitedEndOfDataStatic(eb: ElementBase)
  extends LiteralNilDelimitedEndOfData(eb) with StaticDelim
case class LiteralNilDelimitedEndOfDataDynamic(eb: ElementBase)
  extends LiteralNilDelimitedEndOfData(eb) with DynamicDelim

case class LogicalNilValue(e: ElementBase) extends Primitive(e, e.isNillable)

