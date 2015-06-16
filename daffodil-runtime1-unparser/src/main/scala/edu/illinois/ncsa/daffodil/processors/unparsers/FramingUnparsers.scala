package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.PrimUnparser

class SkipRegionUnparser(
  alignmentInBits: Int,
  skipInBits: Int,
  e: RuntimeData)
  extends AlignmentFillUnparser(alignmentInBits, e) {

  override def unparse(state: UState) = {
    super.unparse(state)
    val dos = state.dataOutputStream
    if (!dos.skip(skipInBits)) UE(state, "Unable to skip %s(bits).", skipInBits)
  }
}

class AlignmentFillUnparser(
  alignmentInBits: Int,
  e: RuntimeData)
  extends PrimUnparser(e) {

  def unparse(state: UState): Unit = {
    val dos = state.dataOutputStream
    if (!dos.align(alignmentInBits)) UE(state, "Unable to align to %s(bits).", alignmentInBits)
  }
}