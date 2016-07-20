package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.FillByteEv
import edu.illinois.ncsa.daffodil.processors.SuspendableOperation

class SkipRegionUnparser(
  skipInBits: Int,
  e: RuntimeData,
  fillByteEv: FillByteEv)
    extends PrimUnparserObject(e) {

  override def unparse(state: UState) = {
    val dos = state.dataOutputStream
    if (!dos.skip(skipInBits)) UE(state, "Unable to skip %s(bits).", skipInBits)
  }
}

class AlignmentFillUnparser(
  alignmentInBits: Int,
  override val rd: RuntimeData,
  fillByteEv: FillByteEv)
    extends PrimUnparserObject(rd)
    with SuspendableOperation {

  override def test(ustate: UState) = {
    val dos = ustate.dataOutputStream
    dos.maybeAbsBitPos0b.isDefined
  }

  override def continuation(state: UState) {
    val dos = state.dataOutputStream
    val fb = fillByteEv.evaluate(state)
    dos.setFillByte(fb)
    if (!dos.align(alignmentInBits))
      UE(state, "Unable to align to %s(bits).", alignmentInBits)
  }

  override def unparse(state: UState): Unit = {
    run(state)
  }
}

class MandatoryTextAlignmentUnparser(
  alignmentInBits: Int,
  e: RuntimeData,
  fillByteEv: FillByteEv)
    extends AlignmentFillUnparser(alignmentInBits, e, fillByteEv)
