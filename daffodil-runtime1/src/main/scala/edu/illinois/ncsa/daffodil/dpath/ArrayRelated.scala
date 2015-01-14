package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.processors.DINode

case object FNCount extends RecipeOp {
  override def run(dstate: DState) {
    dstate.setCurrentValue(dstate.arrayLength)
  }
}

/**
 * Returns \$arg if it contains exactly one item. Otherwise, raises an error
 */
case object FNExactlyOne extends RecipeOp {
  override def run(dstate: DState) {

    //      // Original implementation with boolean return-type
    ////    val bool = dstate.arrayLength == 1
    ////    dstate.setCurrentValue(bool)
    //
    //    // New implementation as stated in tickets DFDL-1085/1087
    //    val hasExactlyOne = dstate.arrayLength == 1
    //    if (hasExactlyOne) {
    //      val array = dstate.currentArray
    //      val item = array.getOccurrence(1).asInstanceOf[DINode]
    //     
    //      dstate.setCurrentNode(item)
    //    }
    //    else { throw new Exception("fn:exactly-one called with a sequence containing zero or more than one item.") }

  }
}

case object DFDLOccursIndex extends RecipeOp {
  override def run(dstate: DState) {
    dstate.setCurrentValue(dstate.pstate.mpstate.arrayPos)
  }
}