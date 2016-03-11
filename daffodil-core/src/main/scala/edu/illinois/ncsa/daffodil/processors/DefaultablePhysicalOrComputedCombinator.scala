package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.Found
import edu.illinois.ncsa.daffodil.dsom.NotFound

case class DefaultablePhysicalOrComputed(ctxt: ElementBase,
  scalarDefaultablePhysical: Gram,
  inputValueCalcElement: Gram,
  outputValueCalcElement: Gram,
  defaultableElement: Gram)
  extends Terminal(ctxt, true) {
  //Assert.invariant(!scalarDefaultablePhysical.isEmpty)
  //Assert.invariant(!inputValueCalcElement.isEmpty)
  //Assert.invariant(!outputValueCalcElement.isEmpty)
  //Assert.invariant(!defaultableElement.isEmpty) // NYI? So this will be empty?

  lazy val scalarDefaultablePhysicalParser = scalarDefaultablePhysical.parser
  lazy val inputValueCalcElementParser = inputValueCalcElement.parser
  lazy val defaultableElementParser = defaultableElement.parser

  lazy val scalarDefaultablePhysicalUnparser = scalarDefaultablePhysical.unparser
  lazy val outputValueCalcElementUnparser = outputValueCalcElement.unparser
  lazy val defaultableElementUnparser = defaultableElement.unparser

  override lazy val parser = {
    (ctxt.inputValueCalcOption, ctxt.outputValueCalcOption) match {
      case (_: NotFound, _: Found) => scalarDefaultablePhysicalParser // outputValueCalc element is just a regular physical element for parser
      case (_: Found, _: NotFound) => inputValueCalcElementParser
      case _ => Assert.impossibleCase()
    }
  }

  override lazy val unparser = {
    val ivcOpt = ctxt.inputValueCalcOption
    val ovcOpt = ctxt.outputValueCalcOption
    (ivcOpt, ovcOpt) match {
      case (_: NotFound, _: Found) => outputValueCalcElementUnparser
      // when unparsing, inputValueCalc elements don't contribute to the data.
      // They may get referenced from outputValueCalc or other expressions so their
      // element values may need to be in the infoset
      case (_: Found, _: NotFound) => defaultableElementUnparser
      case _ => Assert.impossibleCase()
    }
  }

}
