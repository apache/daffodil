package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.dsom.Term
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.AlignmentUnits
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.RuntimeData

class LeadingSkipRegionParser(
  alignment: Int,
  leadingSkip: Int,
  e: RuntimeData)
  extends PrimParser(e) {
  def parse(pstate: PState) = {
    // Is there a reason why we can't do alignment * leadingSkip before this step?
    // Doesn't follow PEMDAS?
    val newBitPos = alignment * leadingSkip + pstate.bitPos
    pstate.withPos(newBitPos, -1, Nope)
  }
}

class TrailingSkipRegionParser(
  alignment: Int,
  trailingSkip: Int,
  e: RuntimeData)
  extends PrimParser(e) {
  def parse(pstate: PState) = {
    val newBitPos = alignment * trailingSkip + pstate.bitPos
    pstate.withPos(newBitPos, -1, Nope)
  }
}

class AlignmentFillParser(
  alignment: Any,
  alignmentInBits: Int,
  e: RuntimeData)
  extends PrimParser(e) {

  def isAligned(currBitPos: Long): Boolean = {
    if (alignmentInBits == 0 || currBitPos == 0) return true
    if ((currBitPos - alignmentInBits) < 0) return false
    if ((currBitPos % alignmentInBits) == 0) return true
    return false
  }

  def parse(pstate: PState) = {
    if (!isAligned(pstate.bitPos)) {
      val maxBitPos = pstate.bitPos + alignmentInBits - 1
      val newBitPos = maxBitPos - maxBitPos % alignmentInBits
      pstate.withPos(newBitPos, -1, Nope)
    } else
      pstate
  }
}