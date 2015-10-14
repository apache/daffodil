package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Maybe

case class SimpleNilOrEmptyOrValueUnparser(ctxt: RuntimeData,
    nilUnparser: Unparser, emptyUnparser: Unparser, valueUnparser: Unparser) extends Unparser(ctxt) {

  override lazy val childProcessors = Seq(nilUnparser, emptyUnparser, valueUnparser)

  def unparse(state: UState): Unit = {
    Assert.invariant(Maybe.isDefined(state.currentInfosetNode))
    val inode = state.currentInfosetNode.asSimple
    if (inode.isNilled) nilUnparser.unparse(state)
    else if (inode.isEmpty) emptyUnparser.unparse(state)
    else valueUnparser.unparse(state)
  }
}

case class SimpleNilOrValueUnparser(ctxt: RuntimeData,
    nilUnparser: Unparser, valueUnparser: Unparser) extends Unparser(ctxt) {

  override lazy val childProcessors = Seq(nilUnparser, valueUnparser)

  def unparse(state: UState): Unit = {
    Assert.invariant(Maybe.isDefined(state.currentInfosetNode))
    val inode = state.currentInfosetNode.asSimple
    if (inode.isNilled) nilUnparser.unparse(state)
    else valueUnparser.unparse(state)
  }
}

case class SimpleEmptyOrValueUnparser(ctxt: RuntimeData,
    emptyUnparser: Unparser, valueUnparser: Unparser) extends Unparser(ctxt) {

  override lazy val childProcessors = Seq(emptyUnparser, valueUnparser)

  def unparse(state: UState): Unit = {
    Assert.invariant(Maybe.isDefined(state.currentInfosetNode))
    val inode = state.currentInfosetNode.asSimple
    if (inode.isEmpty) emptyUnparser.unparse(state)
    else valueUnparser.unparse(state)
  }
}

case class ComplexNilOrContentUnparser(ctxt: RuntimeData,
    nilUnparser: Unparser, contentUnparser: Unparser) extends Unparser(ctxt) {

  override lazy val childProcessors = Seq(nilUnparser, contentUnparser)

  def unparse(state: UState): Unit = {
    Assert.invariant(Maybe.isDefined(state.currentInfosetNode))
    val inode = state.currentInfosetNode.asComplex
    if (inode.isNilled) nilUnparser.unparse(state)
    else contentUnparser.unparse(state)
  }
}
