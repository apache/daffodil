package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.PrimUnparser
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData

class EncodingChangeUnparser(val termRuntimeData: TermRuntimeData)
  extends PrimUnparser(termRuntimeData) with TextUnparserRuntimeMixin {

  def unparse(state: UState): Unit = {
    setupEncoder(state, termRuntimeData)
  }

}