package edu.illinois.ncsa.daffodil.grammar

import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.dsom.Term

trait AlignedMixin extends GrammarMixin { self: Term =>
  lazy val leadingSkipRegion = Prod("leadingSkipRegion", this, leadingSkip > 0, LeadingSkipRegion(this))
  lazy val trailingSkipRegion = Prod("trailingSkipRegion", this, trailingSkip > 0, TrailingSkipRegion(this))
  lazy val alignmentFill = Prod("alignmentFill", this, !isKnownPreAligned, AlignmentFill(this))

  /**
   * true if we can statically determine that the start of this
   * will be properly aligned by where the prior thing left us positioned.
   * Hence we are guaranteed to be properly aligned.
   */
  // TODO: make this actually do the position analysis - that however, requires computing
  // known alignment information based on the starting known alignment and known length
  // of prior things (recursively). I.e., it's a bit tricky.
  lazy val isKnownPreAligned = self.encodingInfo.isScannable || (alignment == 1 && alignmentUnits == AlignmentUnits.Bits)

  // TODO: deal with case of a bit field that is not a multiple of bytes wide
  // but has a terminator which is text and so has mandatory alignment.
  //  /**
  //   * Region of up to 7 bits to get us to a byte boundary for text.
  //   */
  //  lazy val initiatorAlign = Prod("initiatorAlign", this, !isInitiatorPreAligned, TextAlign(mandatoryAlignment))
  //  lazy val terminatorAlign = Prod("terminatorAlign", this, !isTerminatorPreAligned, TextAlign(mandatoryAlignment))
  //  lazy val separatorAlign = Prod("separatorAlign", this, !isSeparatorPreAligned, TextAlign(mandatoryAlignment))
  //  
  //  lazy val isInitiatorPreAligned = {
  //    if (!hasInitiator) true
  //    else {
  //      alignmentCompatible(precedingTermAlignment, mandatoryAlignment)
  //    }
  //  }

  lazy val hasNoSkipRegions = leadingSkip == 0 && trailingSkip == 0

}

