package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.dfa._
import edu.illinois.ncsa.daffodil.util.LogLevel
import scala.collection.mutable.Queue
import edu.illinois.ncsa.daffodil.dsom.EntityReplacer
import edu.illinois.ncsa.daffodil.processors.dfa.CreateDelimiterDFA
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.DataLoc
import edu.illinois.ncsa.daffodil.dsom.ListOfStringValueAsLiteral
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.processors.TextReader
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.dsom.RuntimeEncodingMixin
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.YesNo
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.processors.EncodingInfo

trait HasDelimiterText {

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

abstract class DelimiterValues extends HasDelimiterText with Serializable

class StaticTextDelimiterValues(
  val delim: String,
  allTerminatingMarkup: List[(CompiledExpression, String, String)],
  encInfo: EncodingInfo,
  context: RuntimeData)
  extends DelimiterValues {

  val staticTexts = delim.split("\\s").toList
  val staticTextsCooked: Queue[String] = new Queue

  staticTexts.foreach(x => staticTextsCooked.enqueue(EntityReplacer { _.replaceAll(x, Some(context)) }))

  val delimsRaw = allTerminatingMarkup.map {
    case (delimValue, elemName, elemPath) => (delimValue.constantAsString, elemName, elemPath)
  }
  val delimsCookedWithPosition = delimsRaw.map {
    case (delimValue, elemName, elemPath) => {
      (new ListOfStringValueAsLiteral(delimValue.toString, context).cooked, elemName, elemPath)
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
  val textParser = new TextParser(context, encInfo)
}

class DynamicTextDelimiterValues(
  val delimExpr: CompiledExpression,
  allTerminatingMarkup: List[(CompiledExpression, String, String)],
  encodingInfo: EncodingInfo,
  context: ThrowsSDE)
  extends DelimiterValues {

  // If there are any static delimiters, pre-process them here
  lazy val staticDelimsRaw =
    allTerminatingMarkup.filter {
      case (delimValue, _, _) => delimValue.isConstant
    }.map {
      case (delimValue, eName, ePath) => (delimValue.constantAsString, eName, ePath)
    }
  lazy val staticDelimsCookedWithPosition = staticDelimsRaw.map {
    case (delimValue, elemName, elemPath) => { (new ListOfStringValueAsLiteral(delimValue.toString, context).cooked, elemName, elemPath) }
  }
  lazy val staticDelimsCooked = staticDelimsCookedWithPosition.map { case (delimValue, _, _) => delimValue }.flatten

  val constantLocalDelimsCooked: Maybe[List[String]] = delimExpr.isConstant match {
    case false => Nope
    case true => {
      val cookedResult = new ListOfStringValueAsLiteral(delimExpr.constantAsString, context).cooked
      One(cookedResult)
    }
  }
  val allStaticDelims = {
    val localDelimsCooked = if (constantLocalDelimsCooked.isDefined) { constantLocalDelimsCooked.get } else { Seq.empty }
    val allDelims = staticDelimsCooked.union(localDelimsCooked).toSet
    allDelims
  }
  val maxDelimLengthStatic = computeMaxDelimiterLength(allStaticDelims)
}

abstract class DelimiterTextParser(rd: RuntimeData,
  val encodingInfo: EncodingInfo)
  extends PrimParser(rd)
  with HasDelimiterText {

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

class StaticTextParser(
  rd: RuntimeData,
  delimValues: StaticTextDelimiterValues,
  kindString: String,
  textParser: TextParser,
  positionalInfo: String,
  encInfo: EncodingInfo)
  extends DelimiterTextParser(rd, encInfo)
  with TextReader
  with RuntimeEncodingMixin {

  override def toBriefXML(depthLimit: Int = -1) = {
    "<" + kindString + ">" + delimValues.delim + " " + delimValues.delimsRaw + "</" + kindString + ">"
  }

  override def toString = kindString + "('" + delimValues.delim + "')" //  with terminating markup: " + term.prettyTerminatingMarkup + ")"

  def isRemoteText(originalRepresentation: String): Boolean =
    delimValues.remoteDelims.find(local => local == originalRepresentation).isDefined

  def isLocalText(originalRepresentation: String): Boolean =
    delimValues.staticTextsCooked.find(local => local == originalRepresentation).isDefined

  def parse(start: PState): PState = withParseErrorThrowing(start) {

    val bytePos = (start.bitPos >> 3).toInt

    val reader = getReader(dcharset.charset, start.bitPos, start)

    if (!start.mpstate.foundDelimiter.isDefined) {
      textParser.delims = delimValues.delims
      val result = textParser.parse(reader, true)
      if (!result.isDefined) {
        val foundInstead = computeValueFoundInsteadOfDelimiter(start, delimValues.maxDelimLength)

        return PE(start, "%s - %s: Delimiter not found!  Was looking for (%s) but found \"%s\" instead.",
          this.toString(), rd.prettyName, delimValues.allDelims.mkString(", "), foundInstead)
      } else {
        val res = result.get
        if (isRemoteText(res.originalDelimiterRep)) {
          val (remoteDelimValue, remoteElemName, remoteElemPath) =
            getMatchedDelimiterInfo(res.originalDelimiterRep, delimValues.delimsCookedWithPosition)

          return PE(start, "%s - %s: Found delimiter (%s) for %s when looking for %s(%s) for %s %s",
            this.toString(), rd.prettyName, remoteDelimValue, remoteElemPath, kindString, delimValues.staticTexts.mkString(" "), rd.path, positionalInfo)
        } else {
          val numBits = res.numBits
          val endCharPos = if (start.charPos == -1) res.numCharsRead else start.charPos + res.numCharsRead
          val endBitPosDelim = numBits + start.bitPos

          return start.withPos(endBitPosDelim, endCharPos, Some(res.next))
        }
      }
    } else {
      val found = start.mpstate.foundDelimiter.get
      val isRT = isRemoteText(found.originalRepresentation)
      val isLT = isLocalText(found.originalRepresentation)

      if (isRT && !isLT) {
        val (remoteDelimValue, remoteElemName, remoteElemPath) =
          getMatchedDelimiterInfo(found.originalRepresentation, delimValues.delimsCookedWithPosition)

        return PE(start, "%s - %s: Found delimiter (%s) for %s when looking for %s(%s) for %s %s",
          this.toString(), rd.prettyName, remoteDelimValue, remoteElemPath, kindString, delimValues.staticTexts.mkString(" "), rd.path, positionalInfo)
      } else if (!isRT && !isLT) {
        val foundInstead = computeValueFoundInsteadOfDelimiter(start, delimValues.maxDelimLength)
        return PE(start, "%s - %s: Delimiter not found!  Was looking for (%s) but found \"%s\" instead.",
          this.toString(), rd.prettyName, delimValues.allDelims.mkString(", "), foundInstead)
      } else {
        val numBits = knownEncodingStringBitLength(found.foundText)
        val endCharPos = if (start.charPos == -1) found.foundText.length() else start.charPos + found.foundText.length()
        val endBitPosDelim = numBits + start.bitPos

        val state = start.withPos(endBitPosDelim, endCharPos, Some(reader.atBitPos(endBitPosDelim)))
        state.mpstate.clearDelimitedText
        return state
      }
    }
  }
}

class DynamicTextParser(
  rd: RuntimeData,
  delimExpr: CompiledExpression,
  delimValues: DynamicTextDelimiterValues,
  kindString: String,
  textParser: TextParser,
  positionalInfo: String,
  allTerminatingMarkup: List[(CompiledExpression, String, String)],
  encInfo: EncodingInfo)
  extends DelimiterTextParser(rd, encInfo)
  with TextReader
  with RuntimeEncodingMixin {
  override def toBriefXML(depthLimit: Int = -1) = {
    "<" + kindString + ">" + delimExpr + " " + delimExpr + "</" + kindString + ">"
  }

  Assert.invariant(delimExpr.toString != "") // shouldn't be here at all in this case.
  override def toString = kindString + "('" + delimExpr + "')" //  with terminating markup: " + term.prettyTerminatingMarkup + ")"

  def parse(start: PState): PState = withParseErrorThrowing(start) {
    // We must feed variable context out of one evaluation and into the next.
    // So that the resulting variable map has the updated status of all evaluated variables.
    var pstate = start

    val dynamicDelimsRaw = allTerminatingMarkup.filter { case (delimValue, elemName, elemPath) => !delimValue.isConstant }.map {
      case (delimValue, elemName, elemPath) =>
        {
          val (res, newVMap) = delimValue.evaluate(pstate)
          pstate = pstate.withVariables(newVMap)
          (res, elemName, elemPath)
        }
    }
    // Dynamic delimiters can only be evaluated at runtime
    val dynamicDelimsCookedWithPosition = dynamicDelimsRaw.map {
      case (delimValue, elemValue, elemPath) => { (new ListOfStringValueAsLiteral(delimValue.toString, rd).cooked, elemValue, elemPath) }
    }
    val dynamicDelimsCooked = dynamicDelimsCookedWithPosition.map { case (delimValue, _, _) => delimValue }.flatten
    val delimsCooked = dynamicDelimsCooked.union(delimValues.staticDelimsCooked)

    val localDelimsCookedWithPosition = {
      if (delimValues.constantLocalDelimsCooked.isDefined) { delimValues.constantLocalDelimsCooked.get }
      else {
        val (res, newVMap) = delimExpr.evaluate(pstate)
        pstate = pstate.withVariables(newVMap)
        val cookedResult = new ListOfStringValueAsLiteral(res.toString(), rd).cooked
        cookedResult
      }
    }

    val localDelimsCooked = localDelimsCookedWithPosition

    val remoteDelimsCooked = dynamicDelimsCooked.diff(localDelimsCooked)

    def isRemoteText(originalRepresentation: String): Boolean =
      remoteDelimsCooked.find(remote => remote == originalRepresentation).isDefined

    def isLocalText(originalRepresentation: String): Boolean =
      localDelimsCooked.find(remote => remote == originalRepresentation).isDefined

    val postEvalState = pstate

    val bytePos = (postEvalState.bitPos >> 3).toInt

    val reader = getReader(dcharset.charset, start.bitPos, postEvalState)

    if (!start.mpstate.foundDelimiter.isDefined) {
      val allDynamicDelims = {
        val localDynamicDelims = if (delimValues.constantLocalDelimsCooked.isDefined) { Seq.empty } else { localDelimsCooked }
        localDynamicDelims.toSet.union(dynamicDelimsCooked.toSet)
      }
      val allDelims = delimValues.allStaticDelims.union(allDynamicDelims).toSeq
      val delims = CreateDelimiterDFA(allDelims)
      textParser.delims = delims
      val result = textParser.parse(reader, true)
      if (!result.isDefined) {
        val maxDelimLengthDynamic = computeMaxDelimiterLength(allDynamicDelims)
        val maxDelimLength = Seq(maxDelimLengthDynamic, delimValues.maxDelimLengthStatic).max

        val foundInstead = computeValueFoundInsteadOfDelimiter(start, maxDelimLength)
        return PE(start, "%s - %s: Delimiter not found!  Was looking for (%s) but found \"%s\" instead.",
          this.toString(), rd.prettyName, allDelims.mkString(", "), foundInstead)
      } else {
        val res = result.get
        if (isRemoteText(res.originalDelimiterRep)) {
          val (remoteDelimValue, remoteElemName, remoteElemPath) =
            getMatchedDelimiterInfo(res.originalDelimiterRep, delimValues.staticDelimsCookedWithPosition ::: dynamicDelimsCookedWithPosition)

          return PE(start, "%s - %s: Found delimiter (%s) for %s when looking for %s(%s) for %s %s",
            this.toString(), rd.prettyName, remoteDelimValue, remoteElemPath, kindString, localDelimsCooked.mkString(" "), rd.path, positionalInfo)
        } else {
          val numBits = res.numBits
          val endCharPos = if (start.charPos == -1) res.numCharsRead else start.charPos + res.numCharsRead
          val endBitPosDelim = numBits + start.bitPos

          return start.withPos(endBitPosDelim, endCharPos, Some(res.next))
        }
      }
    } else {
      val found = start.mpstate.foundDelimiter.get
      if (isRemoteText(found.originalRepresentation) && !isLocalText(found.originalRepresentation)) {
        val (remoteDelimValue, remoteElemName, remoteElemPath) =
          getMatchedDelimiterInfo(found.originalRepresentation, delimValues.staticDelimsCookedWithPosition ::: dynamicDelimsCookedWithPosition)

        return PE(start, "%s - %s: Found delimiter (%s) for %s when looking for %s(%s) for %s %s",
          this.toString(), rd.prettyName, remoteDelimValue, remoteElemPath, kindString, localDelimsCooked.mkString(" "), rd.path, positionalInfo)
      } else if (!isRemoteText(found.originalRepresentation) && !isLocalText(found.originalRepresentation)) {
        val allDynamicDelims = {
          val localDynamicDelims = if (delimValues.constantLocalDelimsCooked.isDefined) { Seq.empty } else { localDelimsCooked }
          localDynamicDelims.toSet.union(dynamicDelimsCooked.toSet)
        }
        val allDelims = delimValues.allStaticDelims.union(allDynamicDelims).toSeq
        val foundInstead = computeValueFoundInsteadOfDelimiter(start, delimValues.maxDelimLengthStatic)
        return PE(start, "%s - %s: Delimiter not found!  Was looking for (%s) but found \"%s\" instead.",
          this.toString(), rd.prettyName, allDelims.mkString(", "), foundInstead)
      } else {
        val numBits = knownEncodingStringBitLength(found.foundText)
        val endCharPos = if (start.charPos == -1) found.foundText.length() else start.charPos + found.foundText.length()
        val endBitPosDelim = numBits + start.bitPos

        val state = start.withPos(endBitPosDelim, endCharPos, Some(reader.atBitPos(endBitPosDelim)))
        state.mpstate.clearDelimitedText
        return state
      }
    }
  }
}
