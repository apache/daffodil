package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.BitOrderChangeMixin
import edu.illinois.ncsa.daffodil.processors.PrimUnparser
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData
import edu.illinois.ncsa.daffodil.processors.BinaryParserUnparserRuntimeMixin

class ByteOrderChangeUnparser(val termRuntimeData: TermRuntimeData, byteOrdExpr: CompiledExpression)
  extends PrimUnparser(termRuntimeData) with BinaryParserUnparserRuntimeMixin {

  def unparse(state: UState): Unit = {
    setupByteOrder(state, termRuntimeData, byteOrdExpr)
  }

}