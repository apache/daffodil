package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.dsom.Term
import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.dsom.SchemaComponent
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.dpath.AsIntConverters

abstract class PrimParser(contextArg: RuntimeData)
  extends DaffodilParser(contextArg)
  with WithParseErrorThrowing {

  override def toBriefXML(depthLimit: Int = -1): String = {
    "<" + Misc.getNameFromClass(this) + "/>"
  }

  override def toString = toBriefXML()

}

class NadaParser(context: RuntimeData) extends PrimParser(context) {
  override def toString = "Nada"

  def parse(start: PState): PState = start
}

case class Nada(sc: Term) extends Terminal(sc, true) {
  override def isEmpty = false
  // cannot optimize this out! It is used as an alternative to things
  // with the intention of "find this and this, or find nothing"

  override lazy val parser = new NadaParser(sc.runtimeData)
}
