package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.processors.dfa.DFADelimiter

object EmptyDelimiterStackUnparseNode {
  val node = new DelimiterStackUnparseNode(Nope, Nope, Nope, Nope, Nope, Nope)
  def apply() = node
}

object DelimiterStackUnparseNode {

  def apply(
    initiator: Maybe[DFADelimiter],
    separator: Maybe[DFADelimiter],
    terminator: Maybe[DFADelimiter],
    initiatorLoc: Maybe[(String, String)],
    separatorLoc: Maybe[(String, String)],
    terminatorLoc: Maybe[(String, String)]): DelimiterStackUnparseNode = {
    if (!initiator.isDefined && !terminator.isDefined && !separator.isDefined) EmptyDelimiterStackUnparseNode()
    else new DelimiterStackUnparseNode(initiator, separator, terminator, initiatorLoc, separatorLoc, terminatorLoc)
  }

}

class DelimiterStackUnparseNode(
  val initiator: Maybe[DFADelimiter],
  val separator: Maybe[DFADelimiter],
  val terminator: Maybe[DFADelimiter],
  val initiatorLoc: Maybe[(String, String)],
  val separatorLoc: Maybe[(String, String)],
  val terminatorLoc: Maybe[(String, String)]) {

}