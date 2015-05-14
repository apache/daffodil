/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors.parsers

import scala.annotation.migration
import scala.collection.mutable.Queue
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.dsom.EntityReplacer
import edu.illinois.ncsa.daffodil.dsom.ListOfStringValueAsLiteral
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.processors.DataLoc
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.processors.TextReader
import edu.illinois.ncsa.daffodil.processors.dfa.CreateDelimiterDFA
import edu.illinois.ncsa.daffodil.processors.dfa.TextParser
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.util.Maybe.toMaybe
import edu.illinois.ncsa.daffodil.util.Enum
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData
import java.nio.charset.StandardCharsets

trait ComputesValueFoundInstead {
  //
  // TODO: DFDL-1370 - This really should be replaced by having the DFA that matches delimiters
  // extract the non-matching data found instead. This way of doing it separately is expensive
  // and cannot be avoided every time we backtrack due to a non-found delimiter.
  //
  def computeValueFoundInsteadOfDelimiter(state: PState, maxDelimiterLength: Int): String = {
    val ei = state.getContext().encodingInfo
    val cs = if (ei.isKnownEncoding) ei.knownEncodingCharset.charset else StandardCharsets.UTF_8 // guess utf8
    var rdr = state.inStream.getCharReader(cs, state.bitPos0b)
    val sb = new StringBuilder(maxDelimiterLength)
    var i = 0
    while (i < maxDelimiterLength && !rdr.atEnd) {
      i += 1
      sb + rdr.first
      rdr = rdr.rest
    }
    val foundInstead = sb.mkString
    foundInstead
  }
}

abstract class DelimiterValues extends Serializable

class InitiatorDelimiterValues(val init: String, context: TermRuntimeData)
  extends StaticTextDelimiterValues(init, List.empty, context)

class StaticTextDelimiterValues(
  val delim: String,
  allTerminatingMarkup: List[(CompiledExpression, String, String)],
  context: TermRuntimeData)
  extends DelimiterValues {

  val delimsRaw = allTerminatingMarkup.map {
    case (delimValue, elemName, elemPath) => (delimValue.constantAsString, elemName, elemPath)
  }

  val textParser = new TextParser(context)
}

object DelimiterTextType extends Enum {
  abstract sealed trait Type extends EnumValueType
  case object Initiator extends Type
  case object Separator extends Type
  case object Terminator extends Type
}

abstract class DelimiterTextParserBase(rd: TermRuntimeData,
  delimiterType: DelimiterTextType.Type)
  extends PrimParser(rd)
  with ComputesValueFoundInstead {

  val isInitiator: Boolean = delimiterType == DelimiterTextType.Initiator

  def isLocalText(originalRepresentation: String, state: PState): Boolean =
    if (delimiterType == DelimiterTextType.Initiator) state.mpstate.localDelimiters.existsInInitiator(originalRepresentation)
    else if (delimiterType == DelimiterTextType.Separator) state.mpstate.localDelimiters.existsInSeparator(originalRepresentation)
    else state.mpstate.localDelimiters.existsInTerminator(originalRepresentation)

  def isRemoteText(originalRepresentation: String, state: PState): Boolean = !isLocalText(originalRepresentation, state)

  def hasLocalES(state: PState): Boolean = state.mpstate.localDelimiters.hasEmptyString(delimiterType)
  def hasRemoteES(state: PState): Boolean = {
    val remote = state.mpstate.remoteDelimiters
    remote.find(r => r.hasEmptyString(delimiterType)).isDefined
  }

  def getMatchedDelimiterInfo(originalDelimRep: String,
    state: PState) = {

    val delimiters = state.mpstate.getAllDelimitersWithPos
    val (remoteDelimValue, remoteElemName, remoteElemPath, _) =
      {
        val findResult = delimiters.map {
          case (delimValueList, elemName, elemPath) => {
            val res = delimValueList.find(delim => delim == originalDelimRep) match {
              case Some(d) => (d, elemName, elemPath, true)
              case None => (delimValueList.mkString(","), elemName, elemPath, false)
            }
            res
          }
        }.toSet.filter { x => x._4 == true }

        if (findResult.size == 0) Assert.impossibleCase()
        findResult.head
      }
    (remoteDelimValue, remoteElemName, remoteElemPath)
  }
}

class DelimiterTextParser(
  rd: TermRuntimeData,
  delimExpr: CompiledExpression,
  kindString: String,
  textParser: TextParser,
  positionalInfo: String,
  delimiterType: DelimiterTextType.Type)
  extends DelimiterTextParserBase(rd, delimiterType)
  with TextReader {

  Assert.invariant(delimExpr.toString != "") // shouldn't be here at all in this case.

  override lazy val nom = kindString
  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..."
    else "<" + nom + ">" + delimExpr + "</" + nom + ">"
  }
  override def toString = kindString + "('" + delimExpr + "')"

  override def parse(start: PState): Unit = withParseErrorThrowing(start) {

    val localDelimsCooked = start.mpstate.localDelimiters.getAllDelimiters

    val bytePos = (start.bitPos >> 3).toInt

    val reader = getReader(rd.encodingInfo.knownEncodingCharset.charset, start.bitPos, start)

    if (isInitiator || !start.mpstate.foundDelimiter.isDefined) {
      val delims = {
        val localDelims = if (delimiterType == DelimiterTextType.Initiator) start.mpstate.localDelimiters.getInitiators.getOrElse(Assert.impossible("Initiator should always find at least one initiator on stack."))
        else if (delimiterType == DelimiterTextType.Separator) start.mpstate.localDelimiters.getSeparators.getOrElse(Assert.impossible("Separator should always find at least one separator on stack."))
        else start.mpstate.localDelimiters.getTerminators.getOrElse(Assert.impossible("Terminator should always find at least one terminator on stack."))
        val remoteDelims = start.mpstate.getAllTerminatingMarkup
        localDelims ++ remoteDelims
      }

      val result = textParser.parse(reader, delims, true)

      if (!result.isDefined) {
        if (hasLocalES(start)) {
          // found local ES, regardless if there was a remote ES
          val numBits = 0
          val endCharPos = start.charPos
          val endBitPosDelim = numBits + start.bitPos

          start.setPos(endBitPosDelim, endCharPos, Some(reader.atBitPos(endBitPosDelim)))
          start.mpstate.clearDelimitedText
          return
        } else if (hasRemoteES(start)) {
          // has remote but not local (PE)
          val (remoteDelimValue, remoteElemName, remoteElemPath) =
            getMatchedDelimiterInfo("%ES;", start)

          PE(start, "%s - %s: Found delimiter (%s) for %s when looking for %s(%s) for %s %s",
            this.toString(), rd.prettyName, remoteDelimValue, remoteElemPath,
            kindString, localDelimsCooked.mkString(" "), rd.path, positionalInfo)
          return
        } else {
          // no match and no ES in delims
          val maxDelimLength = start.mpstate.localDelimiters.getMaxDelimiterLength

          val foundInstead = computeValueFoundInsteadOfDelimiter(start, maxDelimLength)
          PE(start, "%s - %s: Delimiter not found!  Was looking for (%s) but found \"%s\" instead.",
            this.toString(), rd.prettyName, localDelimsCooked.mkString(", "), foundInstead)
          return
        }
      } else {
        val res = result.get

        if (!isLocalText(res.originalDelimiterRep, start)) {
          val (remoteDelimValue, remoteElemName, remoteElemPath) =
            getMatchedDelimiterInfo(res.originalDelimiterRep,
              start)

          PE(start, "%s - %s: Found delimiter (%s) for %s when looking for %s(%s) for %s %s",
            this.toString(), rd.prettyName, remoteDelimValue, remoteElemPath,
            kindString, localDelimsCooked.mkString(" "), rd.path, positionalInfo)
          return
        }

        val numBits = res.numBits
        val endCharPos = if (start.charPos == -1) res.numCharsRead else start.charPos + res.numCharsRead
        val endBitPosDelim = numBits + start.bitPos

        start.setPos(endBitPosDelim, endCharPos, Some(res.next))
        return
      }
    } else {
      val found = start.mpstate.foundDelimiter.get
      if (!isLocalText(found.originalRepresentation, start)) {
        val (remoteDelimValue, remoteElemName, remoteElemPath) =
          getMatchedDelimiterInfo(found.originalRepresentation,
            start)

        PE(start, "%s - %s: Found delimiter (%s) for %s when looking for %s(%s) for %s %s",
          this.toString(), rd.prettyName, remoteDelimValue, remoteElemPath,
          kindString, localDelimsCooked.mkString(" "), rd.path, positionalInfo)
        return
      } else {
        val numBits = rd.encodingInfo.knownEncodingStringBitLength(found.foundText)
        val endCharPos = if (start.charPos == -1) found.foundText.length() else start.charPos + found.foundText.length()
        val endBitPosDelim = numBits + start.bitPos

        start.setPos(endBitPosDelim, endCharPos, Some(reader.atBitPos(endBitPosDelim)))
        start.mpstate.clearDelimitedText
      }
    }

  }
}
