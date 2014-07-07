package edu.illinois.ncsa.daffodil.processors.parsers

import java.nio.charset.Charset

import edu.illinois.ncsa.daffodil.dsom.SchemaComponent
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.util.Maybe


abstract class HexBinaryLengthInBytesParser(justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  gram: Gram,
  contextArg: SchemaComponent)
  extends StringLengthInBytesParser(justificationTrim: TextJustificationType.Type,
    pad: Maybe[Char],
    gram: Gram,
    contextArg: SchemaComponent) {

  override val charset: Charset = Charset.forName("ISO-8859-1")
  override val stringLengthInBitsFnc = {
    def stringBitLength(str: String) = {
      // variable width encoding, so we have to convert each character 
      // We assume here that it will be a multiple of bytes
      // that is, that variable-width encodings are all some number
      // of bytes.
      str.getBytes(charset).length * 8
    }
    stringBitLength _
  }
  override def formatValue(value: String) = {
    val hexStr = value.map(c => c.toByte.formatted("%02X")).mkString
    hexStr
  }
}

class HexBinaryFixedLengthInBytesParser(nBytes: Long,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  gram: Gram,
  contextArg: SchemaComponent)
  extends HexBinaryLengthInBytesParser(justificationTrim: TextJustificationType.Type,
    pad: Maybe[Char],
    gram: Gram,
    contextArg: SchemaComponent) {

  lazy val parserName = "HexBinaryFixedLengthInBytes"
  lazy val lengthText = e.length.constantAsString

  def getLength(pstate: PState): (Long, PState) = {
    (nBytes, pstate)
  }
}

class HexBinaryFixedLengthInBitsParser(nBits: Long,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  gram: Gram,
  contextArg: SchemaComponent)
  extends HexBinaryLengthInBytesParser(justificationTrim: TextJustificationType.Type,
    pad: Maybe[Char],
    gram: Gram,
    contextArg: SchemaComponent) {

  lazy val parserName = "HexBinaryFixedLengthInBits"
  lazy val lengthText = e.length.constantAsString

  def getLength(pstate: PState): (Long, PState) = {
    val nBytes = scala.math.ceil(nBits / 8).toLong
    (nBytes, pstate)
  }
}

class HexBinaryVariableLengthInBytesParser(justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  gram: Gram,
  contextArg: SchemaComponent)
  extends HexBinaryLengthInBytesParser(justificationTrim: TextJustificationType.Type,
    pad: Maybe[Char],
    gram: Gram,
    contextArg: SchemaComponent) with HasVariableLength {

  lazy val parserName = "HexBinaryVariableLengthInBytes"
  lazy val lengthText = exprText
}