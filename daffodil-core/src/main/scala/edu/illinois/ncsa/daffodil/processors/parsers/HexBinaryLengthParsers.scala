package edu.illinois.ncsa.daffodil.processors.parsers

import java.nio.charset.Charset
import edu.illinois.ncsa.daffodil.dsom.SchemaComponent
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset

abstract class HexBinaryLengthInBytesParser(justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData)
  extends StringLengthInBytesParser(
    justificationTrim,
    pad,
    erd,
    new DFDLCharset("ISO-8859-1"),
    true,
    8,
    "ISO-8859-1") {
  
  override def formatValue(value: String) = {
    val hexStr = value.map(c => c.toByte.formatted("%02X")).mkString
    hexStr
  }
}

class HexBinaryFixedLengthInBytesParser(nBytes: Long,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  override val lengthText: String)
  extends HexBinaryLengthInBytesParser(justificationTrim, pad, erd) {

  lazy val parserName = "HexBinaryFixedLengthInBytes"

  def getLength(pstate: PState): (Long, PState) = {
    (nBytes, pstate)
  }
}

class HexBinaryFixedLengthInBitsParser(nBits: Long,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  override val lengthText: String)
  extends HexBinaryLengthInBytesParser(justificationTrim, pad, erd) {

  lazy val parserName = "HexBinaryFixedLengthInBits"

  def getLength(pstate: PState): (Long, PState) = {
    val nBytes = scala.math.ceil(nBits / 8).toLong
    (nBytes, pstate)
  }
}

class HexBinaryVariableLengthInBytesParser(justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  codepointWidth: Int,
  override val length: CompiledExpression,
  override val lengthText: String)
  extends HexBinaryLengthInBytesParser(justificationTrim, pad, erd)
  with HasVariableLength {

  lazy val parserName = "HexBinaryVariableLengthInBytes"
}
