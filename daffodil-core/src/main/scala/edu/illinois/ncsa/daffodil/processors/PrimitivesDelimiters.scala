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

abstract class StaticDelimiter(kindString: String, delim: String, e: Term, eb: Term, guard: Boolean = true)
  extends StaticText(delim, e, eb, kindString, guard)

abstract class StaticText(delim: String, e: Term, eb: Term, kindString: String, guard: Boolean = true)
  extends Text(e, eb, guard) //extends DelimParserBase(e, guard)
  with DelimiterText
  with WithParseErrorThrowing with TextReader {

  val charset = e.knownEncodingCharset
  val term = e.asInstanceOf[Term]

  val staticTexts = delim.split("\\s").toList
  val staticTextsCooked: Queue[String] = new Queue

  staticTexts.foreach(x => staticTextsCooked.enqueue(EntityReplacer.replaceAll(x, Some(e))))

  val delimsRaw = e.allTerminatingMarkup.map {
    case (delimValue, elemName, elemPath) => (delimValue.constantAsString, elemName, elemPath)
  }
  val delimsCookedWithPosition = delimsRaw.map {
    case (delimValue, elemName, elemPath) => {
      (new ListOfStringValueAsLiteral(delimValue.toString, e).cooked, elemName, elemPath)
    }
  }
  val delimsCooked = delimsCookedWithPosition.map { case (delimValue, _, _) => delimValue }.flatten

  // Here we expect that remoteDelims shall be defined as those delimiters who are not
  // also defined locally.  That is to say that local should win over remote.
  val remoteDelims = delimsCooked.toSet.diff(staticTextsCooked.toSet)

  val allDelims = staticTextsCooked.toSet.union(remoteDelims.toSet)
  val maxDelimLength = computeMaxDelimiterLength(allDelims)

  // here we define the parsers so that they are pre-compiled/generated
  val delims = CreateDelimiterDFA(allDelims.toSeq)
  val textParser = new TextParser(e.knownEncodingStringBitLengthFunction)

  Assert.invariant(delim != "") // shouldn't be here at all in this case.

  e.schemaDefinitionWarningUnless(e.ignoreCase == YesNo.No, "Property ignoreCase='yes' not supported.")

  def isRemoteText(originalRepresentation: String): Boolean =
    remoteDelims.find(local => local == originalRepresentation).isDefined

  def isLocalText(originalRepresentation: String): Boolean =
    staticTextsCooked.find(local => local == originalRepresentation).isDefined

  def parser: DaffodilParser = new PrimParser(this, e) {

    override def toBriefXML(depthLimit: Int = -1) = {
      "<" + kindString + ">" + delim + " " + delimsRaw + "</" + kindString + ">"
    }

    override def toString = kindString + "('" + delim + "')" //  with terminating markup: " + term.prettyTerminatingMarkup + ")"

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

        start.foundDelimiter match {
          case None => {
            textParser.delims = delims
            val result = textParser.parse(reader, true)
            result match {
              case None => {
                val foundInstead = computeValueFoundInsteadOfDelimiter(start, maxDelimLength)
                log(LogLevel.Debug, "%s - %s: Delimiter not found!  Was looking for (%s) but found \"%s\" instead.",
                  this.toString(), eName, allDelims.mkString(", "), foundInstead)
                return PE(start, "%s - %s: Delimiter not found!  Was looking for (%s) but found \"%s\" instead.",
                  this.toString(), eName, allDelims.mkString(", "), foundInstead)
              }
              case Some(res) if isRemoteText(res.originalDelimiterRep) => {
                val (remoteDelimValue, remoteElemName, remoteElemPath) =
                  getMatchedDelimiterInfo(res.originalDelimiterRep, delimsCookedWithPosition)

                log(LogLevel.Debug, "%s - %s: Found delimiter (%s) for %s when looking for %s(%s) for %s %s",
                  this.toString(), eName, remoteDelimValue, remoteElemPath, kindString, staticTexts.mkString(" "), e.path, positionalInfo)
                return PE(start, "%s - %s: Found delimiter (%s) for %s when looking for %s(%s) for %s %s",
                  this.toString(), eName, remoteDelimValue, remoteElemPath, kindString, staticTexts.mkString(" "), e.path, positionalInfo)
              }
              case Some(res) => {
                val numBits = res.numBits
                val endCharPos = if (start.charPos == -1) res.numCharsRead else start.charPos + res.numCharsRead
                val endBitPosDelim = numBits + start.bitPos

                log(LogLevel.Debug, "%s - Found %s", eName, res.matchedDelimiterValue.get)
                log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPosDelim >> 3))
                log(LogLevel.Debug, "%s - Ended at bit position %s", eName, endBitPosDelim)

                return start.withPos(endBitPosDelim, endCharPos, Some(res.next))
              }
            }

          }
          case Some(found) if isRemoteText(found.originalRepresentation) && !isLocalText(found.originalRepresentation) => {
            val (remoteDelimValue, remoteElemName, remoteElemPath) =
              getMatchedDelimiterInfo(found.originalRepresentation, delimsCookedWithPosition)

            log(LogLevel.Debug, "%s - %s: Found delimiter (%s) for %s when looking for %s(%s) for %s %s",
              this.toString(), eName, remoteDelimValue, remoteElemPath, kindString, staticTexts.mkString(" "), e.path, positionalInfo)
            return PE(start, "%s - %s: Found delimiter (%s) for %s when looking for %s(%s) for %s %s",
              this.toString(), eName, remoteDelimValue, remoteElemPath, kindString, staticTexts.mkString(" "), e.path, positionalInfo)
          }
          case Some(found) if !isRemoteText(found.originalRepresentation) && !isLocalText(found.originalRepresentation) => {
            val foundInstead = computeValueFoundInsteadOfDelimiter(start, maxDelimLength)
            log(LogLevel.Debug, "%s - %s: Delimiter not found!  Was looking for (%s) but found \"%s\" instead.",
              this.toString(), eName, allDelims.mkString(", "), foundInstead)
            return PE(start, "%s - %s: Delimiter not found!  Was looking for (%s) but found \"%s\" instead.",
              this.toString(), eName, allDelims.mkString(", "), foundInstead)
          }
          case Some(found) => {
            val numBits = e.knownEncodingStringBitLengthFunction(found.foundText)
            val endCharPos = if (start.charPos == -1) found.foundText.length() else start.charPos + found.foundText.length()
            val endBitPosDelim = numBits + start.bitPos
            log(LogLevel.Debug, "%s - Found %s", eName, found.foundText)
            log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPosDelim >> 3))
            log(LogLevel.Debug, "%s - Ended at bit position %s", eName, endBitPosDelim)

            val state = start.withPos(endBitPosDelim, endCharPos, Some(reader.atBitPos(endBitPosDelim)))
            return state.clearDelimitedText
          }
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

abstract class Text(es: Term, e: Term, guard: Boolean) extends DelimParserBase(es, guard) {
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
  with DelimiterText
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
  lazy val staticDelimsCookedWithPosition = staticDelimsRaw.map {
    case (delimValue, elemName, elemPath) => { (new ListOfStringValueAsLiteral(delimValue.toString, e).cooked, elemName, elemPath) }
  }
  lazy val staticDelimsCooked = staticDelimsCookedWithPosition.map { case (delimValue, _, _) => delimValue }.flatten

  val constantLocalDelimsCooked: Option[List[String]] = delimExpr.isConstant match {
    case false => None
    case true => {
      val cookedResult = new ListOfStringValueAsLiteral(delimExpr.constantAsString, e).cooked
      Some(cookedResult)
    }
  }
  val allStaticDelims = {
    val localDelimsCooked = if (constantLocalDelimsCooked.isDefined) { constantLocalDelimsCooked.get } else { Seq.empty }
    val allDelims = staticDelimsCooked.union(localDelimsCooked).toSet
    allDelims
  }
  val maxDelimLengthStatic = computeMaxDelimiterLength(allStaticDelims)

  val textParser = new TextParser(e.knownEncodingStringBitLengthFunction)

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
        val dynamicDelimsCookedWithPosition = dynamicDelimsRaw.map {
          case (delimValue, elemValue, elemPath) => { (new ListOfStringValueAsLiteral(delimValue.toString, e).cooked, elemValue, elemPath) }
        }
        val dynamicDelimsCooked = dynamicDelimsCookedWithPosition.map { case (delimValue, _, _) => delimValue }.flatten
        val delimsCooked = dynamicDelimsCooked.union(staticDelimsCooked)

        val localDelimsCookedWithPosition = {
          if (constantLocalDelimsCooked.isDefined) { constantLocalDelimsCooked.get }
          else {
            val R(res, newVMap) = delimExpr.evaluate(start.parentElement, vars, start)
            vars = newVMap
            val cookedResult = new ListOfStringValueAsLiteral(res.toString(), e).cooked
            cookedResult
          }
        }

        val localDelimsCooked = localDelimsCookedWithPosition

        val remoteDelimsCooked = dynamicDelimsCooked.diff(localDelimsCooked)

        def isRemoteText(originalRepresentation: String): Boolean =
          remoteDelimsCooked.find(remote => remote == originalRepresentation).isDefined

        def isLocalText(originalRepresentation: String): Boolean =
          localDelimsCooked.find(remote => remote == originalRepresentation).isDefined

        val postEvalState = start.withVariables(vars)

        log(LogLevel.Debug, "%s - Parsing delimiter at byte position: %s", eName, (postEvalState.bitPos >> 3))
        log(LogLevel.Debug, "%s - Parsing delimiter at bit position: %s", eName, postEvalState.bitPos)

        log(LogLevel.Debug, "%s - Looking for local(%s) not remote (%s).", eName, localDelimsCooked.toSet, delimsCooked.toSet)

        val bytePos = (postEvalState.bitPos >> 3).toInt

        log(LogLevel.Debug, "Retrieving reader state.")
        val reader = getReader(charset, start.bitPos, postEvalState)

        start.foundDelimiter match {
          case None => {
            val allDynamicDelims = {
              val localDynamicDelims = if (constantLocalDelimsCooked.isDefined) { Seq.empty } else { localDelimsCooked }
              localDynamicDelims.toSet.union(dynamicDelimsCooked.toSet)
            }
            val allDelims = allStaticDelims.union(allDynamicDelims).toSeq
            val delims = CreateDelimiterDFA(allDelims)
            textParser.delims = delims
            val result = textParser.parse(reader, true)
            result match {
              case None => {
                val maxDelimLengthDynamic = computeMaxDelimiterLength(allDynamicDelims)
                val maxDelimLength = Seq(maxDelimLengthDynamic, maxDelimLengthStatic).max

                val foundInstead = computeValueFoundInsteadOfDelimiter(start, maxDelimLength)
                log(LogLevel.Debug, "%s - %s: Delimiter not found!  Was looking for (%s) but found \"%s\" instead.",
                  this.toString(), eName, allDelims.mkString(", "), foundInstead)
                return PE(start, "%s - %s: Delimiter not found!  Was looking for (%s) but found \"%s\" instead.",
                  this.toString(), eName, allDelims.mkString(", "), foundInstead)
              }
              case Some(res) if isRemoteText(res.originalDelimiterRep) => {
                val (remoteDelimValue, remoteElemName, remoteElemPath) =
                  getMatchedDelimiterInfo(res.originalDelimiterRep, staticDelimsCookedWithPosition ::: dynamicDelimsCookedWithPosition)

                log(LogLevel.Debug, "%s - %s: Found delimiter (%s) for %s when looking for %s(%s) for %s %s",
                  this.toString(), eName, remoteDelimValue, remoteElemPath, kindString, localDelimsCooked.mkString(" "), e.path, positionalInfo)
                return PE(start, "%s - %s: Found delimiter (%s) for %s when looking for %s(%s) for %s %s",
                  this.toString(), eName, remoteDelimValue, remoteElemPath, kindString, localDelimsCooked.mkString(" "), e.path, positionalInfo)
              }
              case Some(res) => {
                val numBits = res.numBits
                val endCharPos = if (start.charPos == -1) res.numCharsRead else start.charPos + res.numCharsRead
                val endBitPosDelim = numBits + start.bitPos

                log(LogLevel.Debug, "%s - Found %s", eName, res.matchedDelimiterValue.get)
                log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPosDelim >> 3))
                log(LogLevel.Debug, "%s - Ended at bit position %s", eName, endBitPosDelim)

                return start.withPos(endBitPosDelim, endCharPos, Some(res.next))
              }
            }
          }
          case Some(found) if isRemoteText(found.originalRepresentation) && !isLocalText(found.originalRepresentation) => {
            val (remoteDelimValue, remoteElemName, remoteElemPath) =
              getMatchedDelimiterInfo(found.originalRepresentation, staticDelimsCookedWithPosition ::: dynamicDelimsCookedWithPosition)

            log(LogLevel.Debug, "%s - %s: Found delimiter (%s) for %s when looking for %s(%s) for %s %s",
              this.toString(), eName, remoteDelimValue, remoteElemPath, kindString, localDelimsCooked.mkString(" "), e.path, positionalInfo)
            return PE(start, "%s - %s: Found delimiter (%s) for %s when looking for %s(%s) for %s %s",
              this.toString(), eName, remoteDelimValue, remoteElemPath, kindString, localDelimsCooked.mkString(" "), e.path, positionalInfo)
          }
          case Some(found) if !isRemoteText(found.originalRepresentation) && !isLocalText(found.originalRepresentation) => {
            val allDynamicDelims = {
              val localDynamicDelims = if (constantLocalDelimsCooked.isDefined) { Seq.empty } else { localDelimsCooked }
              localDynamicDelims.toSet.union(dynamicDelimsCooked.toSet)
            }
            val allDelims = allStaticDelims.union(allDynamicDelims).toSeq
            val foundInstead = computeValueFoundInsteadOfDelimiter(start, maxDelimLengthStatic)
            log(LogLevel.Debug, "%s - %s: Delimiter not found!  Was looking for (%s) but found \"%s\" instead.",
              this.toString(), eName, allDelims.mkString(", "), foundInstead)
            return PE(start, "%s - %s: Delimiter not found!  Was looking for (%s) but found \"%s\" instead.",
              this.toString(), eName, allDelims.mkString(", "), foundInstead)
          }
          case Some(found) => {
            val numBits = e.knownEncodingStringBitLengthFunction(found.foundText)
            val endCharPos = if (start.charPos == -1) found.foundText.length() else start.charPos + found.foundText.length()
            val endBitPosDelim = numBits + start.bitPos
            log(LogLevel.Debug, "%s - Found %s", eName, found.foundText)
            log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPosDelim >> 3))
            log(LogLevel.Debug, "%s - Ended at bit position %s", eName, endBitPosDelim)

            val state = start.withPos(endBitPosDelim, endCharPos, Some(reader.atBitPos(endBitPosDelim)))
            return state.clearDelimitedText
          }
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
      //      val localDelimsCookedWithPosition = new ListOfStringValueAsLiteral(localDelimsRaw.toString(), e).cooked
      //      val localDelimsCooked = localDelimsCookedWithPosition
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
  val dp = new DFDLDelimParser(e.knownEncodingStringBitLengthFunction)

  private def isPrefixOf(possiblePrefix: String, string: String): Boolean = {
    string.startsWith(possiblePrefix)
  }

  private def checkPadCharDistinctness(
    padChar: Option[String],
    escChar: Option[String],
    escEscChar: Option[String],
    escBlockStart: Option[String],
    escBlockEnd: Option[String],
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
    escChar: Option[String],
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
    escEscChar: Option[String],
    escBlockStart: Option[String],
    escBlockEnd: Option[String],
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
    escBlockStart: Option[String],
    escBlockEnd: Option[String],
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
    padChar: Option[String],
    escChar: Option[String],
    escEscChar: Option[String],
    escBlockStart: Option[String],
    escBlockEnd: Option[String],
    terminatingMarkup: Seq[String],
    context: ThrowsSDE): Unit = {

    checkPadCharDistinctness(padChar, escChar, escEscChar, escBlockStart, escBlockEnd, terminatingMarkup, context)
    checkEscCharDistinctness(escChar, terminatingMarkup, context)
    checkEscEscCharDistinctness(escEscChar, escBlockStart, escBlockEnd, terminatingMarkup, context)
    checkEscapeBlockDistinctness(escBlockStart, escBlockEnd, terminatingMarkup, context)
  }

  def checkDelimiterDistinctness(
    escapeSchemeKind: EscapeSchemeKind.Type,
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
