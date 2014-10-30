package edu.illinois.ncsa.daffodil.dpath

case object FNCount extends RecipeOp {
  override def run(dstate: DState) {
    dstate.setCurrentValue(dstate.arrayLength)
  }
}

case object DFDLOccursIndex extends RecipeOp {
  override def run(dstate: DState) {
    dstate.setCurrentValue(dstate.pstate.mpstate.arrayPos)
  }
}