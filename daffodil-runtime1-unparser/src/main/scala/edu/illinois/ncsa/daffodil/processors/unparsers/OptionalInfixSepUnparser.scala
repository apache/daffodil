package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.PrimUnparser
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.nio.charset.MalformedInputException
import edu.illinois.ncsa.daffodil.processors.DelimiterType


class OptionalInfixSepUnparser(contextArg: RuntimeData,
  sepUnparser: Unparser)
  extends PrimUnparser(contextArg) {

  def unparse(state: UState): Unit = {
    if (state.arrayPos > 1) sepUnparser.unparse1(state, contextArg)
    else if (state.groupPos > 1) sepUnparser.unparse1(state, contextArg)
  }
}