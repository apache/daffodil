package edu.illinois.ncsa.daffodil.processors

import scala.collection.mutable.Queue
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.processors.dfa.DFADelimiter
import edu.illinois.ncsa.daffodil.processors.parsers.DelimiterTextType
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._

object EmptyDelimiterStackNode {
  val node = new DelimiterStackNode(Seq.empty, Seq.empty, Seq.empty, Nope, Nope, Nope)
  def apply() = node
}

object DelimiterStackNode {

  def apply(
    initiators: Seq[DFADelimiter],
    separators: Seq[DFADelimiter],
    terminators: Seq[DFADelimiter],
    initiatorLoc: Maybe[(String, String)],
    separatorLoc: Maybe[(String, String)],
    terminatorLoc: Maybe[(String, String)]): DelimiterStackNode = {
    if (initiators.isEmpty && terminators.isEmpty && separators.isEmpty) EmptyDelimiterStackNode()
    else new DelimiterStackNode(initiators, separators, terminators, initiatorLoc, separatorLoc, terminatorLoc)
  }

}

class DelimiterStackNode(
  initiators: Seq[DFADelimiter],
  separators: Seq[DFADelimiter],
  terminators: Seq[DFADelimiter],
  initiatorLoc: Maybe[(String, String)],
  separatorLoc: Maybe[(String, String)],
  terminatorLoc: Maybe[(String, String)]) {

  def existsInTerminatingMarkup(value: String): Boolean = terminatingMarkup.find(d => d.lookingFor == value).isDefined
  def existsInInitiator(value: String): Boolean = initiators.find(d => d.lookingFor == value).isDefined
  def existsInSeparator(value: String): Boolean = separators.find(d => d.lookingFor == value).isDefined
  def existsInTerminator(value: String): Boolean = terminators.find(d => d.lookingFor == value).isDefined

  def getInitiators = if (initiators.isEmpty) None else Some(initiators)
  def getSeparators = if (separators.isEmpty) None else Some(separators)
  def getTerminators = if (terminators.isEmpty) None else Some(terminators)

  def getTerminatingMarkup = terminatingMarkup
  def getTerminatingMarkupWithPos = terminatingMarkupWithPos
  def getDelimitersWithPos = delimitersWithPos

  def getMaxDelimiterLength = maxDelimiterLength
  def getAllDelimiters = allDelimiters

  def hasEmptyString(delimiterType: DelimiterTextType.Type): Boolean = {
    if (delimiterType == DelimiterTextType.Initiator) initiatorHasES
    else if (delimiterType == DelimiterTextType.Separator) separatorHasES
    else terminatorHasES
  }

  override def toString() =
    "<DelimStackNode initiators=\"" + theInitiators + "\"" +
      "terminatingMarkup=\" " + terminatingMarkup.mkString(",") + "\"/>"

  private val maxLengthForVariableLengthDelimiter = DaffodilTunableParameters.maxLengthForVariableLengthDelimiterDisplay

  private lazy val maxDelimiterLength = {
    val allDelims = delimiters.map(d => d.lookingFor).toSet
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
  private lazy val terminatingMarkup = separators ++ terminators
  private lazy val terminatingMarkupWithPos = {
    val tm = Queue.empty[(List[String], String, String)]

    if (!separators.isEmpty) {
      val (elemVal, elemPath) = separatorLoc.get
      tm.enqueue((separators.map(d => d.lookingFor).toList, elemVal, elemPath))
    }

    if (!terminators.isEmpty) {
      val (elemVal, elemPath) = terminatorLoc.get
      tm.enqueue((terminators.map(d => d.lookingFor).toList, elemVal, elemPath))
    }

    tm.toList
  }
  private lazy val allDelimiters = delimiters.map(d => d.lookingFor)
  private lazy val delimiters = initiators ++ terminatingMarkup
  private lazy val delimitersWithPos = {
    val q = Queue.empty[(List[String], String, String)]

    if (!initiators.isEmpty) {
      val (elemVal, elemPath) = initiatorLoc.get
      q.enqueue((initiators.map(d => d.lookingFor).toList, elemVal, elemPath))
    }

    q.toList ++ terminatingMarkupWithPos
  }
  private lazy val theInitiators = if (initiators.isEmpty) "" else initiators.mkString(",")

  private lazy val initiatorHasES = initiators.find(i => i.lookingFor == "%ES;").isDefined
  private lazy val separatorHasES = separators.find(i => i.lookingFor == "%ES;").isDefined
  private lazy val terminatorHasES = terminators.find(i => i.lookingFor == "%ES;").isDefined

}