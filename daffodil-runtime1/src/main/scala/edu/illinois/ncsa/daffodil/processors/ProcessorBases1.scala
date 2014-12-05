package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }
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
