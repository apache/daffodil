package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.processors.parsers.SimpleNilOrEmptyOrValueParser
import edu.illinois.ncsa.daffodil.processors.unparsers.SimpleNilOrEmptyOrValueUnparser
import edu.illinois.ncsa.daffodil.processors.parsers.SimpleNilOrValueParser
import edu.illinois.ncsa.daffodil.processors.unparsers.SimpleNilOrValueUnparser
import edu.illinois.ncsa.daffodil.processors.parsers.SimpleEmptyOrValueParser
import edu.illinois.ncsa.daffodil.processors.unparsers.SimpleEmptyOrValueUnparser
import edu.illinois.ncsa.daffodil.processors.parsers.ComplexNilOrContentParser
import edu.illinois.ncsa.daffodil.processors.unparsers.ComplexNilOrContentUnparser
import edu.illinois.ncsa.daffodil.grammar.Terminal

case class SimpleNilOrEmptyOrValue(ctxt: ElementBase, nilGram: Gram, emptyGram: Gram, valueGram: Gram) extends Terminal(ctxt, true) {
  Assert.invariant(!nilGram.isEmpty)
  Assert.invariant(!emptyGram.isEmpty)
  Assert.invariant(!valueGram.isEmpty)

  lazy val nilParser = nilGram.parser
  lazy val emptyParser = emptyGram.parser
  lazy val valueParser = valueGram.parser

  lazy val nilUnparser = nilGram.unparser
  lazy val emptyUnparser = emptyGram.unparser
  lazy val valueUnparser = valueGram.unparser

  override def parser = SimpleNilOrEmptyOrValueParser(context.runtimeData, nilParser, emptyParser, valueParser)

  override def unparser = SimpleNilOrEmptyOrValueUnparser(context.runtimeData, nilUnparser, emptyUnparser, valueUnparser)

}

case class SimpleNilOrValue(ctxt: ElementBase, nilGram: Gram, valueGram: Gram) extends Terminal(ctxt, true) {
  Assert.invariant(!nilGram.isEmpty)
  Assert.invariant(!valueGram.isEmpty)

  lazy val nilParser = nilGram.parser
  lazy val valueParser = valueGram.parser

  lazy val nilUnparser = nilGram.unparser
  lazy val valueUnparser = valueGram.unparser

  override def parser = SimpleNilOrValueParser(context.runtimeData, nilParser, valueParser)

  override def unparser = SimpleNilOrValueUnparser(context.runtimeData, nilUnparser, valueUnparser)

}

case class SimpleEmptyOrValue(ctxt: ElementBase, emptyGram: Gram, valueGram: Gram) extends Terminal(ctxt, true) {
  Assert.invariant(!emptyGram.isEmpty)
  Assert.invariant(!valueGram.isEmpty)

  lazy val emptyParser = emptyGram.parser
  lazy val valueParser = valueGram.parser

  lazy val emptyUnparser = emptyGram.unparser
  lazy val valueUnparser = valueGram.unparser

  override def parser = SimpleEmptyOrValueParser(context.runtimeData, emptyParser, valueParser)

  override def unparser = SimpleEmptyOrValueUnparser(context.runtimeData, emptyUnparser, valueUnparser)

}

case class ComplexNilOrContent(ctxt: ElementBase, nilGram: Gram, contentGram: Gram) extends Terminal(ctxt, true) {
  Assert.invariant(!nilGram.isEmpty)
  Assert.invariant(!contentGram.isEmpty)

  lazy val nilParser = nilGram.parser
  lazy val contentParser = contentGram.parser

  lazy val nilUnparser = nilGram.unparser
  lazy val contentUnparser = contentGram.unparser

  override def parser = ComplexNilOrContentParser(context.runtimeData, nilParser, contentParser)

  override def unparser = ComplexNilOrContentUnparser(context.runtimeData, nilUnparser, contentUnparser)

}
