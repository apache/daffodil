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

package edu.illinois.ncsa.daffodil.grammar.primitives

import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.Implicits._;
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.processors.parsers.{ Parser => DaffodilParser }
import edu.illinois.ncsa.daffodil.processors.unparsers.{ Unparser => DaffodilUnparser }
import edu.illinois.ncsa.daffodil.processors.dfa.TextParser
import edu.illinois.ncsa.daffodil.processors.parsers.DelimiterTextParser
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind
import edu.illinois.ncsa.daffodil.processors.parsers.DelimiterTextType
import edu.illinois.ncsa.daffodil.processors.unparsers.DelimiterTextUnparser

object INoWarn5 { ImplicitsSuppressUnusedImportWarning() }

abstract class Text(es: Term, e: Term, guard: Boolean) extends StringDelimBase(es, guard) {

  lazy val eName = e.toString()

  lazy val positionalInfo = {
    if (e.isDirectChildOfSequence) {
      e.nearestEnclosingSequence match {
        case Some(es) => {
          val pos = e.positionInNearestEnclosingSequence - 1
          if (es.hasPrefixSep) {
            if (e.hasPriorRequiredSiblings) {
              val prior: Term = es.groupMembers(pos - 1)
              "after " + prior.diagnosticDebugName + " and before " + eName
            } else "before " + eName
          } else if (es.hasInfixSep)
            if (e.hasPriorRequiredSiblings && e.hasLaterRequiredSiblings) {
              val prior: Term = es.groupMembers(pos - 1)

              "after " + prior.diagnosticDebugName + " and before " + eName
            } else if (e.hasPriorRequiredSiblings) {
              val prior: Term = es.groupMembers(pos - 1)
              "after " + prior.diagnosticDebugName + " and before " + eName
            } else if (e.hasLaterRequiredSiblings) {
              val later: Term = es.groupMembers(pos + 1)
              "before " + later.diagnosticDebugName
            } else { "" }
          else if (es.hasPostfixSep)
            if (e.hasPriorRequiredSiblings && e.hasLaterRequiredSiblings) {
              val later: Term = es.groupMembers(pos + 1)

              "after " + eName + " and before " + later.diagnosticDebugName
            } else if (e.hasPriorRequiredSiblings) {
              val prior: Term = es.groupMembers(pos - 1)
              "after " + prior.diagnosticDebugName + " and before " + eName
            } else if (e.hasLaterRequiredSiblings) {
              val later: Term = es.groupMembers(pos + 1)
              "before " + later.diagnosticDebugName
            } else { "" }
          else
            ""
        }
        case None => ""
      }
    } else {
      ""
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

  def getMatchedDelimiterInfo(originalDelimRep: String,
    delimiters: List[(List[String], String, String)]) = {

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

// NOTE: LiteralNil still uses this as it can only be Static/Constant
//
abstract class StaticText(delim: String, e: Term, eb: Term, kindString: String, guard: Boolean = true)
  extends Text(e, eb, guard) {

  Assert.invariant(delim != "") // shouldn't be here at all in this case.

  lazy val textParser = new TextParser(e.termRuntimeData)
}

abstract class DelimiterText(e: Term, eb: Term, guard: Boolean = true)
  extends Text(e, eb, guard) {

  lazy val textParser = new TextParser(e.termRuntimeData)

  def delimiterType: DelimiterTextType.Type

  override lazy val parser: DaffodilParser = new DelimiterTextParser(e.termRuntimeData, textParser, positionalInfo, delimiterType)
  override lazy val unparser: DaffodilUnparser = new DelimiterTextUnparser(e.termRuntimeData, delimiterType)
}

case class Initiator(e: Term) extends DelimiterText(e, e) {
  Assert.invariant(e.hasInitiator)
  val delimiterType: DelimiterTextType.Type = DelimiterTextType.Initiator
}
case class Separator(s: SequenceTermBase, t: Term) extends DelimiterText(s, t) {
  Assert.invariant(s.hasSeparator)
  val delimiterType: DelimiterTextType.Type = DelimiterTextType.Separator
}
case class Terminator(e: Term) extends DelimiterText(e, e) {
  Assert.invariant(e.hasTerminator)
  val delimiterType: DelimiterTextType.Type = DelimiterTextType.Terminator
}

abstract class StringDelimBase(e: Term, guard: Boolean) extends Terminal(e, guard) {
  override def toString = "StringDelimBase[" + name + "]"

  def checkDelimiterDistinctness(
    escapeSchemeKind: EscapeKind,
    optPadChar: Option[String],
    optEscChar: Option[String], // Could be a DFDL expression
    optEscEscChar: Option[String], // Could be a DFDL expression
    optEscBlkStart: Option[String],
    optEscBlkEnd: Option[String],
    terminatingMarkup: Seq[String],
    context: ThrowsSDE): Unit = {

    // TODO: DFDL-451 - After conversing with Mike B. about this, we're putting this on the backburner.
    // Leaving the code here, just commented out the entry point until we can decide what is the appropriate
    // behavior here.
    //
    //    escapeSchemeKind match {
    //      case EscapeSchemeKind.None =>
    //        checkDelimiterDistinctness_(optPadChar, None, None, None, None, terminatingMarkup, context)
    //      case EscapeSchemeKind.Character =>
    //        checkDelimiterDistinctness_(optPadChar, optEscChar, optEscEscChar, None, None, terminatingMarkup, context)
    //      case EscapeSchemeKind.Block =>
    //        checkDelimiterDistinctness_(optPadChar, None, optEscEscChar,
    //          optEscBlkStart, optEscBlkEnd, terminatingMarkup, context)
    //    }
  }
}
