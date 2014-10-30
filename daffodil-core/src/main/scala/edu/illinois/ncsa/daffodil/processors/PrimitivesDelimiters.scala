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
import scala.collection.mutable.Queue
import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.YesNo
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import scala.util.parsing.input.{ Reader }
import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }
import edu.illinois.ncsa.daffodil.processors.dfa.TextParser
import edu.illinois.ncsa.daffodil.processors.dfa.CreateDelimiterDFA
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind
import edu.illinois.ncsa.daffodil.processors.parsers.DynamicTextDelimiterValues
import edu.illinois.ncsa.daffodil.processors.parsers.DynamicTextParser
import edu.illinois.ncsa.daffodil.processors.parsers.StaticTextDelimiterValues
import edu.illinois.ncsa.daffodil.processors.parsers.StaticTextParser

abstract class StaticDelimiter(kindString: String, delim: String, e: Term, eb: Term, guard: Boolean = true)
  extends StaticText(delim, e, eb, kindString, guard)

abstract class StaticText(delim: String, e: Term, eb: Term, kindString: String, guard: Boolean = true)
  extends Text(e, eb, guard) //extends DelimParserBase(e, guard)
  with DelimiterText {

  Assert.invariant(delim != "") // shouldn't be here at all in this case.

  e.schemaDefinitionWarningUnless(e.ignoreCase == YesNo.No, "Property ignoreCase='yes' not supported.")

  lazy val delimValues = new StaticTextDelimiterValues(delim, e.allTerminatingMarkup, e.encodingInfo, e.runtimeData)
  lazy val textParser = new TextParser(e.runtimeData, e.encodingInfo)

  lazy val parser: DaffodilParser = new StaticTextParser(e.runtimeData, delimValues, kindString, textParser, positionalInfo, e.encodingInfo)
}

abstract class Text(es: Term, e: Term, guard: Boolean) extends DelimParserBase(es, guard) {
  lazy val oes = {
    val oes = e.optionEscapeScheme
    oes.foreach { es =>
      e.schemaDefinitionUnless(es.isKnownEscapeCharacter != Some(false), "Runtime expressions for escapeCharacters are not supported.")
      e.schemaDefinitionUnless(es.isKnownEscapeEscapeCharacter != Some(false), "Runtime expressions for escapeEscapeCharacters are not supported.")
    }
    oes
  }

  lazy val eName = e.toString()

  lazy val positionalInfo = {
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

trait DelimiterText {

  val maxLengthForVariableLengthDelimiter = DaffodilTunableParameters.maxLengthForVariableLengthDelimiterDisplay

  def computeMaxDelimiterLength(allDelims: Set[String]): Int = {
    val variableLengthDelims = allDelims.filter(d => d.contains("%WSP*;") || d.contains("%WSP+;"))
    val allDelimsMinusVariableLength = allDelims -- variableLengthDelims

    val maxLengthDelim = {
      val lengths = allDelimsMinusVariableLength.map(_.length)
      val vLengths = variableLengthDelims.map(_.length)

      (variableLengthDelims.size, lengths.size) match {
        case (0, 0) => maxLengthForVariableLengthDelimiter
        case (0, _) => lengths.max
        case (_, 0) => maxLengthForVariableLengthDelimiter
        case (_, _) if vLengths.max > lengths.max => maxLengthForVariableLengthDelimiter
        case (_, _) if vLengths.max <= lengths.max => lengths.max
        case _ => maxLengthForVariableLengthDelimiter
      }
    }
    maxLengthDelim
  }

  def computeValueFoundInsteadOfDelimiter(state: PState, maxDelimiterLength: Int): String = {
    val dl = state.currentLocation.asInstanceOf[DataLoc]
    val foundInstead = dl.utf8Dump(maxDelimiterLength)
    foundInstead
  }
}

abstract class DynamicText(delimExpr: CompiledExpression, e: Term, kindString: String, guard: Boolean = true)
  extends Text(e, e, guard)
  with DelimiterText {

  lazy val delimValues = new DynamicTextDelimiterValues(delimExpr, e.allTerminatingMarkup, e.encodingInfo, e.runtimeData)
  lazy val textParser = new TextParser(e.runtimeData, e.encodingInfo)

  lazy val parser: DaffodilParser = new DynamicTextParser(e.runtimeData, delimExpr, delimValues, kindString, textParser, positionalInfo, e.allTerminatingMarkup, e.encodingInfo)

}

abstract class DynamicDelimiter(kindString: String, delimExpr: CompiledExpression, e: Term, guard: Boolean = true)
  extends DynamicText(delimExpr, e, kindString, guard)

//case class StaticInitiator(e: Term) extends StaticDelimiter(e.initiator.constantAsString, e)
case class StaticInitiator(e: Term) extends StaticDelimiter("Init", e.initiator.constantAsString, e, e) {
  Assert.invariant(e.hasInitiator)
  lazy val unparserDelim = e.initiator.constantAsString.split("""\s""").head
}
//case class StaticTerminator(e : Term) extends StaticDelimiter(e.terminator.constantAsString, e)
case class StaticTerminator(e: Term) extends StaticDelimiter("Term", e.terminator.constantAsString, e, e) {
  Assert.invariant(e.hasTerminator)
  lazy val unparserDelim = e.terminator.constantAsString.split("""\s""").head
}
case class DynamicInitiator(e: Term) extends DynamicDelimiter("Init", e.initiator, e)
case class DynamicTerminator(e: Term) extends DynamicDelimiter("Term", e.terminator, e)

// Note: for a static separator, we pass s, the sequence, because that is where
// the charset encoding comes from. 
case class StaticSeparator(s: Sequence, t: Term) extends StaticDelimiter("Sep", s.separator.constantAsString, s, t) {
  Assert.invariant(s.hasSeparator)
  lazy val unparserDelim = s.separator.constantAsString.split("""\s""").head
}
case class DynamicSeparator(s: Sequence, t: Term) extends DynamicDelimiter("Sep", s.separator, s)

abstract class DelimParserBase(e: Term, guard: Boolean) extends Terminal(e, guard) {
  override def toString = "DelimParserBase[" + name + "]"
  val dp = new DFDLDelimParser(e.runtimeData, e.encodingInfo)

  private def isPrefixOf(possiblePrefix: String, string: String): Boolean = {
    string.startsWith(possiblePrefix)
  }

  private def checkPadCharDistinctness(
    padChar: Maybe[String],
    escChar: Maybe[String],
    escEscChar: Maybe[String],
    escBlockStart: Maybe[String],
    escBlockEnd: Maybe[String],
    terminatingMarkup: Seq[String],
    context: ThrowsSDE): Unit = {

    padChar.foreach(pc => {
      escChar.foreach(ec => {
        if (pc == ec || isPrefixOf(pc, ec))
          context.SDE("The pad character (%s) cannot be the same as (or a prefix of) the escape character.", pc)
      })
      escEscChar.foreach(eec => {
        if (pc == eec || isPrefixOf(pc, eec))
          context.SDE("The pad character (%s) cannot be the same as (or a prefix of) the escape escape character.", pc)
      })
      escBlockStart.foreach(ebs => {
        if (pc == ebs || isPrefixOf(pc, ebs))
          context.SDE("The pad character (%s) cannot be the same as (or a prefix of) the escape block start.", pc)
      })
      escBlockEnd.foreach(ebe => {
        if (pc == ebe || isPrefixOf(pc, ebe))
          context.SDE("The pad character (%s) cannot be the same as (or a prefix of) the escape block end.", pc)
      })
      terminatingMarkup.foreach(tm => {
        if (tm == pc || isPrefixOf(pc, tm)) {
          context.SDE("The pad character (%s) cannot be the same as (or a prefix of) a piece of the terminating markup (%s).", pc, tm)
        }
      })
    })
  }

  private def checkEscCharDistinctness(
    escChar: Maybe[String],
    terminatingMarkup: Seq[String],
    context: ThrowsSDE): Unit = {

    escChar.foreach(ec =>
      terminatingMarkup.foreach(tm => {
        if (tm == ec || isPrefixOf(ec, tm)) {
          context.SDE("The escapeCharacter (%s) cannot be the same as (or a prefix of) a piece of the terminating markup (%s).", ec, tm)
        }
      }))
  }

  private def checkEscEscCharDistinctness(
    escEscChar: Maybe[String],
    escBlockStart: Maybe[String],
    escBlockEnd: Maybe[String],
    terminatingMarkup: Seq[String],
    context: ThrowsSDE): Unit = {

    escEscChar.foreach(eec => {
      // GeneralPurposeFormat seems to disagree with the below, commenting out for now.
      //        escBlockStart match {
      //          case Some(ebs) if eec == ebs || isPrefixOf(eec, ebs) =>
      //            context.SDE("The escapeEscapeCharacter (%s) cannot be the same as (or a prefix of) the escape block start.",eec)
      //          case _ => // Nothing to do
      //        }
      //        escBlockEnd match {
      //          case Some(ebe) if eec == ebe || isPrefixOf(eec, ebe) =>
      //            context.SDE("The escapeEscapeCharacter (%s) cannot be the same as (or a prefix of) the escape block end.",eec)
      //          case _ => // Nothing to do
      //        }
      terminatingMarkup.foreach(tm => {
        if (tm == eec || isPrefixOf(eec, tm)) {
          context.SDE("The escapeEscapeCharacter (%s) cannot be the same as (or a prefix of) a piece of the terminating markup (%s).", eec, tm)
        }
      })
    })
  }

  private def checkEscapeBlockDistinctness(
    escBlockStart: Maybe[String],
    escBlockEnd: Maybe[String],
    terminatingMarkup: Seq[String],
    context: ThrowsSDE): Unit = {

    escBlockStart.foreach(ebs => terminatingMarkup.foreach(tm => {
      if (tm == ebs || isPrefixOf(ebs, tm)) {
        context.SDE("The escapeBlockStart (%s) cannot be the same as (or a prefix of) a piece of the terminating markup.", ebs)
      }
    }))

    escBlockEnd.foreach(ebe => terminatingMarkup.foreach(tm => {
      if (tm == ebe || isPrefixOf(ebe, tm)) {
        context.SDE("The escapeBlockEnd (%s) cannot be the same as (or a prefix of) a piece of the terminating markup.", ebe)
      }
    }))
  }

  private def checkDelimiterDistinctness_(
    padChar: Maybe[String],
    escChar: Maybe[String],
    escEscChar: Maybe[String],
    escBlockStart: Maybe[String],
    escBlockEnd: Maybe[String],
    terminatingMarkup: Seq[String],
    context: ThrowsSDE): Unit = {

    checkPadCharDistinctness(padChar, escChar, escEscChar, escBlockStart, escBlockEnd, terminatingMarkup, context)
    checkEscCharDistinctness(escChar, terminatingMarkup, context)
    checkEscEscCharDistinctness(escEscChar, escBlockStart, escBlockEnd, terminatingMarkup, context)
    checkEscapeBlockDistinctness(escBlockStart, escBlockEnd, terminatingMarkup, context)
  }

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
