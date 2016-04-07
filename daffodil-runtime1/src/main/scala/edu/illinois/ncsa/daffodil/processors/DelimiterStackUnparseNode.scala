package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.processors.dfa.DFADelimiter

object EmptyDelimiterStackUnparseNode {
  val node = new DelimiterStackUnparseNode(Nope, Nope, Nope)
  def apply() = node
}

object DelimiterStackUnparseNode {

  def apply(
    initiator: Maybe[DFADelimiter],
    separator: Maybe[DFADelimiter],
    terminator: Maybe[DFADelimiter]): DelimiterStackUnparseNode = {
    if (!initiator.isDefined && !terminator.isDefined && !separator.isDefined) EmptyDelimiterStackUnparseNode()
    else new DelimiterStackUnparseNode(initiator, separator, terminator)
  }

}

class DelimiterStackUnparseNode(
  val initiator: Maybe[DFADelimiter],
  val separator: Maybe[DFADelimiter],
  val terminator: Maybe[DFADelimiter]) {

}
