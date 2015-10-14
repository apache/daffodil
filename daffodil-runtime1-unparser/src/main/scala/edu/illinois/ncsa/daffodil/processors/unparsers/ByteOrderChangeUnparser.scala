package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.processors.BinaryParserUnparserRuntimeMixin
import edu.illinois.ncsa.daffodil.processors.PrimUnparser
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData

class ByteOrderChangeUnparser(val termRuntimeData: TermRuntimeData, byteOrdExpr: CompiledExpression)
  extends PrimUnparser(termRuntimeData) with BinaryParserUnparserRuntimeMixin {

  def unparse(state: UState): Unit = {
    setupByteOrder(state, termRuntimeData, byteOrdExpr)
  }

}