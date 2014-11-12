package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.processors.DIElement
import edu.illinois.ncsa.daffodil.processors.ParseError
import edu.illinois.ncsa.daffodil.util.Maybe
import Maybe._

case class DAFTrace(recipe: CompiledDPath, msg: String)
  extends FNOneArg(recipe, NodeInfo.AnyType) {

  override def computeValue(str: Any, dstate: DState) = {
    System.err.println("trace " + msg + ":" + str.toString)
    str
  }
}

case object DAFError extends RecipeOp {
  override def run(dstate: DState) {
    val ie = dstate.pstate.infoset.asInstanceOf[DIElement]
    val pe = new ParseError(One(ie.runtimeData.schemaFileLocation), One(dstate.pstate), "The error function was called.")
    throw pe
  }
}