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
import edu.illinois.ncsa.daffodil.dsom.ListOfStringLiteral
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.dfa.CreateDelimiterDFA
import edu.illinois.ncsa.daffodil.processors.dfa.TextParser
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.util.Maybe.toMaybe
import edu.illinois.ncsa.daffodil.util.Enum
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData
import java.nio.charset.StandardCharsets
import edu.illinois.ncsa.daffodil.processors.TextParserRuntimeMixin
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.processors.PrimParser

trait ComputesValueFoundInstead {
  //
  // TODO: DFDL-1370 - This really should be replaced by having the DFA that matches delimiters
  // extract the non-matching data found instead. This way of doing it separately is expensive
  // and cannot be avoided every time we backtrack due to a non-found delimiter.
  //
  def computeValueFoundInsteadOfDelimiter(state: PState, maxDelimiterLength: Int): String = {
    // val ei = state.getContext().encodingInfo
    val foundInstead = state.dataInputStream.getSomeString(maxDelimiterLength)
    val result = if (foundInstead.isDefined) foundInstead.get else ""
    result
  }
}

abstract class DelimiterValues extends Serializable

class InitiatorDelimiterValues(val init: String, context: TermRuntimeData)
  extends StaticTextDelimiterValues(init, List.empty, context)

class StaticTextDelimiterValues(
  val delim: String,
  allTerminatingMarkup: List[(CompiledExpression[String], String, String)],
  context: TermRuntimeData)
  extends DelimiterValues {

  val delimsRaw = allTerminatingMarkup.map {
    case (delimValue, elemName, elemPath) => (delimValue.constant, elemName, elemPath)
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
  extends PrimParser
  with ComputesValueFoundInstead with TextParserRuntimeMixin {

  override lazy val runtimeDependencies = rd.encodingInfo.runtimeDependencies
  override def context = rd

  val isInitiator: Boolean = delimiterType == DelimiterTextType.Initiator

  def isLocalText(originalRepresentation: String, state: PState): Boolean = {
    val res =
      if (delimiterType == DelimiterTextType.Initiator)
        state.mpstate.localDelimiters.existsInInitiator(originalRepresentation)
      else if (delimiterType == DelimiterTextType.Separator)
        state.mpstate.localDelimiters.existsInSeparator(originalRepresentation)
      else
        state.mpstate.localDelimiters.existsInTerminator(originalRepresentation)
    res
  }

  def isRemoteText(originalRepresentation: String, state: PState): Boolean = !isLocalText(originalRepresentation, state)

  def hasLocalES(state: PState): Boolean = state.mpstate.localDelimiters.hasEmptyString(delimiterType)
  def hasRemoteES(state: PState): Boolean = {
    val remote = state.mpstate.remoteDelimiters
    remote.exists(r => r.hasEmptyString(delimiterType))
  }

  def getMatchedDelimiterInfo(originalDelimRep: String,
    state: PState) = {

    val delimiters = state.mpstate.getAllDelimitersWithPos
    val (remoteDelimValue, remoteElemName, remoteElemPath, _) =
      {
        val findResult = delimiters.map {
          case (delimValueList, elemName, elemPath) => {
            val findResult = delimValueList.find(delim => delim == originalDelimRep)
            val res = findResult match {
              case Some(d) => (d, elemName, elemPath, true)
              case None => (delimValueList.mkString(","), elemName, elemPath, false)
            }
            res
          }
        }.toSet.filter { x => x._4 == true }

        if (findResult.size == 0)
          Assert.impossibleCase()
        findResult.head
      }
    (remoteDelimValue, remoteElemName, remoteElemPath)
  }
}

class DelimiterTextParser(
  rd: TermRuntimeData,
  delimExpr: CompiledExpression[String],
  kindString: String,
  textParser: TextParser,
  positionalInfo: String,
  delimiterType: DelimiterTextType.Type)
  extends DelimiterTextParserBase(rd, delimiterType) {

  //Assert.invariant(delimExpr.toString != "") // shouldn't be here at all in this case.

  override lazy val nom = kindString
  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..."
    else "<" + nom + ">" + delimExpr + "</" + nom + ">"
  }
  override def toString = kindString + "('" + delimExpr + "')"

  override def parse(start: PState): Unit = withParseErrorThrowing(start) {

    setupEncoding(start, rd)

    val localDelimsCooked = start.mpstate.localDelimiters.getAllDelimiters

    // val bytePos = (start.bitPos >> 3).toInt

    if (isInitiator || !start.foundDelimiter.isDefined) {
      //
      // We are going to scan for delimiter text.
      //
      // FIXME: This is incorrect. It grabs all local delimiters even if we're last in a group so the separator
      // cannot be relevant, or if we're in the middle of a group with required elements following so
      // the terminator cannot be relevant.
      //
      // Fixing this is going to require the compiler to pre-compute the relevant delimiters for every
      // Term. (relevant delimiters meaning the specific compiled expressions that are relevant.)
      // PERFORMANCE: this should also help performance by eliminating the construction of lists/sets of
      // these things at run time.
      //
      val delims = {
        val localDelims =
          if (delimiterType == DelimiterTextType.Initiator)
            start.mpstate.localDelimiters.getInitiators.getOrElse(Assert.impossible("Initiator should always find at least one initiator on stack."))
          else if (delimiterType == DelimiterTextType.Separator)
            start.mpstate.localDelimiters.getSeparators.getOrElse(Assert.impossible("Separator should always find at least one separator on stack."))
          else
            start.mpstate.localDelimiters.getTerminators.getOrElse(Assert.impossible("Terminator should always find at least one terminator on stack."))

        // FIXME: This is incorrect. It is going to get too many delimiters. See above.
        // Nowever, code below does assume that we always find a match, even to incorrect markup, so fixing this is
        // more than just getting the right set of remote delims here.
        val remoteDelims = // start.mpstate.getAllTerminatingMarkup
          start.mpstate.remoteDelimiters.flatMap { rd =>
            rd.getTerminatingMarkup
          } // FIXME: Performance - this is going to allocate a structure or several even

        localDelims ++ remoteDelims
      }

      val result = textParser.parse(start.dataInputStream, delims, true)

      if (!result.isDefined) {
        //
        // Did not find delimiter.
        //
        // Still can be ok if ES is a delimiter. That's allowed when we're not lengthKind='delimited'
        // That is, it is allowed if the delimiter is just part of the data syntax, but is not being
        // used to determine length.
        //
        if (hasLocalES(start)) {
          // found local ES, regardless if there was a remote ES
          //          val numBits = 0
          //          val endBitPosDelim = numBits + start.bitPos

          start.clearDelimitedText
          return
        } else if (hasRemoteES(start)) {
          // has remote but not local (PE)
          val (remoteDelimValue, _, remoteElemPath) =
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
            this.toString(), rd.prettyName, delims.mkString(", "),
            Misc.remapStringToVisibleGlyphs(foundInstead))
          return
        }
        //
        // end of !result.isDefined
      } else {
        Assert.invariant(result.isDefined)
        //
        // Found a delimiter
        //
        val res = result.get

        if (!isLocalText(res.originalDelimiterRep, start)) {
          //
          // It was a remote delimiter but we should have found a local one.
          //
          // ??? Do not understand why we know it should have been a local one.
          // An element that is last in a group can be terminated by the terminator of some enclosing
          // parent group that is not even the immediate parent.
          //
          val (remoteDelimValue, _, remoteElemPath) =
            getMatchedDelimiterInfo(res.originalDelimiterRep, start)

          PE(start, "%s - %s: Found delimiter (%s) for %s when looking for %s(%s) for %s %s",
            this.toString(), rd.prettyName, remoteDelimValue, remoteElemPath,
            kindString, delims.mkString(" "), rd.path, positionalInfo)
          return
        }
        //
        // It was a local delimiter.
        //
        val nChars = res.matchedDelimiterValue.get.length
        val wasDelimiterTextSkipped = start.dataInputStream.skipChars(nChars)
        Assert.invariant(wasDelimiterTextSkipped)
        start.clearDelimitedText
        return
      }
    } else {
      //
      // A delimiter was found and cached as part of parsing a previous term.
      // We will not scan for a delimiter. We will check that the cached one matches
      // what we expect.
      //
      // An invariant here is that the input stream has NOT been advanced yet
      // past the delimiter. So we have to advance it here.
      //
      val found = start.foundDelimiter.get
      if (!isLocalText(found.originalRepresentation, start)) {
        val (remoteDelimValue, _ /* remoteElemName */ , remoteElemPath) =
          getMatchedDelimiterInfo(found.originalRepresentation, start)
        PE(start, "%s - %s: Found delimiter (%s) for %s when looking for %s for %s %s",
          this.toString(), rd.prettyName, remoteDelimValue, remoteElemPath, localDelimsCooked.mkString(" "), this.context.path, positionalInfo)
        return
      } else {
        val nChars = found.foundText.length
        val wasDelimiterTextSkipped = start.dataInputStream.skipChars(nChars)
        Assert.invariant(wasDelimiterTextSkipped)
        start.clearDelimitedText
      }
    }

  }
}
