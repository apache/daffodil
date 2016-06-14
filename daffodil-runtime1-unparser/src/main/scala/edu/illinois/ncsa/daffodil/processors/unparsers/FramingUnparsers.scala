package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.RuntimeData

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
  extends PrimUnparserObject(e) {

  def unparse(state: UState): Unit = {
    val dos = state.dataOutputStream
    if (dos.maybeAbsBitPos0b.isDefined) {
      if (!dos.align(alignmentInBits)) UE(state, "Unable to align to %s(bits).", alignmentInBits)
    } else {
      // We don't have the absolute bit position, so we can't do alignment.
      // TODO: implement delayed alignment (creates another buffering of DOS and suspends
      // performning the alignment until the absolute bit pos is determined (after some earlier OVC element
      // of variable length, the data for it is computed so we can determine its length).
      e.SDE("Unable to align - preceding variable-length dfdl:outputValueCalc element is not yet defined.")
    }
  }
}
