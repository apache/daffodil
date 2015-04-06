package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors.Parser
import edu.illinois.ncsa.daffodil.processors.AltCompParser
import edu.illinois.ncsa.daffodil.processors.RuntimeData

case class SimpleNilOrEmptyOrValueParser(ctxt: RuntimeData, nilParser: Parser, emptyParser: Parser, valueParser: Parser)
  extends AltCompParser(ctxt, Seq(nilParser, emptyParser, valueParser))

case class SimpleNilOrValueParser(ctxt: RuntimeData, nilParser: Parser, valueParser: Parser)
  extends AltCompParser(ctxt, Seq(nilParser, valueParser))

case class SimpleEmptyOrValueParser(ctxt: RuntimeData, emptyParser: Parser, valueParser: Parser)
  extends AltCompParser(ctxt, Seq(emptyParser, valueParser))