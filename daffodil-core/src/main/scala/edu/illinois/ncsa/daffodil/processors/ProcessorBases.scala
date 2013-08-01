package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.dsom.Term
import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.dsom.SchemaComponent

abstract class PrimParser(gram: Gram, contextArg: SchemaComponent)
  extends DaffodilParser(contextArg)
  with WithParseErrorThrowing {

  def primitive = gram

  override def toBriefXML(depthLimit: Int = -1): String = {
    "<" + gram.name + "/>"
  }

  override def toString = toBriefXML()

}

case class Nada(sc: Term) extends Terminal(sc, true) {
  override def isEmpty = false
  // cannot optimize this out! It is used as an alternative to things
  // with the intention of "find this and this, or find nothing"

  def parser: DaffodilParser = new PrimParser(this, sc) {
    override def toString = "Nada"

    def parse(start: PState): PState = start
  }

  def unparser: Unparser = new Unparser(sc) {
    override def toString = "Nada"

    def unparse(start: UState): UState = start
  }
}
