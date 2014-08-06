package edu.illinois.ncsa.daffodil.processors
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import java.nio.charset.Charset
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.dsom.R
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthUnits

abstract class SpecifiedLengthCombinatorBase(val e: ElementBase, eGram: => Gram)
  extends Terminal(e, true) {

  val eParser = eGram.parser

  def kind: String

  def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<SpecifiedLengthCombinator_" + kind + ">" +
        eParser.toBriefXML(depthLimit - 1) +
        "</SpecifiedLengthCombinator_" + kind + ">"
  }

}

class SpecifiedLengthPattern(e: ElementBase, eGram: => Gram)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "Pattern"

  if (!e.isScannable) e.SDE("Element %s does not meet the requirements of Pattern-Based lengths and Scanability.\nThe element and its children must be representation='text' and share the same encoding.", e.prettyName)
  def parser: Parser = new SpecifiedLengthPatternParser(
    eParser,
    e.elementRuntimeData,
    e.knownEncodingCharset,
    e.lengthPattern,
    e.knownEncodingStringBitLengthFunction)
  def unparser: Unparser = new DummyUnparser(e)

}

class SpecifiedLengthExplicitBitsFixed(e: ElementBase, eGram: => Gram, nBits: Long)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "ExplicitBitsFixed"

  def parser: Parser = new SpecifiedLengthExplicitBitsFixedParser(
    eParser,
    e.elementRuntimeData,
    nBits,
    e.knownEncodingCharset)
  def unparser: Unparser = new DummyUnparser(e)

}

class SpecifiedLengthExplicitBits(e: ElementBase, eGram: => Gram)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "ExplicitBits"

  lazy val toBits = e.lengthUnits match {
    case LengthUnits.Bits => 1
    case LengthUnits.Bytes => 8
    case _ => e.schemaDefinitionError("Binary Numbers must have length units of Bits or Bytes.")
  }

  def parser: Parser = new SpecifiedLengthExplicitBitsParser(
    eParser,
    e.elementRuntimeData,
    e.knownEncodingCharset,
    e.length,
    toBits)
  def unparser: Unparser = new DummyUnparser(e)

}

class SpecifiedLengthExplicitBytesFixed(e: ElementBase, eGram: => Gram, nBytes: Long)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "ExplicitBytesFixed"

  def parser: Parser = new SpecifiedLengthExplicitBytesFixedParser(
    eParser,
    e.elementRuntimeData,
    nBytes,
    e.knownEncodingCharset)
  def unparser: Unparser = new DummyUnparser(e)

}

class SpecifiedLengthExplicitBytes(e: ElementBase, eGram: => Gram)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "ExplicitBytes"

  def parser: Parser = new SpecifiedLengthExplicitBytesParser(
    eParser,
    e.elementRuntimeData,
    e.knownEncodingCharset,
    e.length)
  def unparser: Unparser = new DummyUnparser(e)

}

class SpecifiedLengthExplicitCharactersFixed(e: ElementBase, eGram: => Gram, nChars: Long)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "ExplicitCharactersFixed"

  def parser: Parser = new SpecifiedLengthExplicitCharactersFixedParser(
    eParser,
    e.elementRuntimeData,
    nChars,
    e.knownEncodingCharset,
    e.knownEncodingStringBitLengthFunction)
  def unparser: Unparser = new DummyUnparser(e)

}

class SpecifiedLengthExplicitCharacters(e: ElementBase, eGram: => Gram)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "ExplicitCharacters"

  def parser: Parser = new SpecifiedLengthExplicitCharactersParser(
    eParser,
    e.elementRuntimeData,
    e.knownEncodingCharset,
    e.length,
    e.knownEncodingStringBitLengthFunction)
  def unparser: Unparser = new DummyUnparser(e)

}

abstract class SpecifiedLengthParserBase(eParser: Parser,
  erd: ElementRuntimeData)
  extends PrimParser(erd)
  with WithParseErrorThrowing {

  override def toBriefXML(depthLimit: Int) = eParser.toBriefXML(depthLimit)

  final def parse(pstate: PState, endBitPos: Long) = {
    log(LogLevel.Debug, "Limiting data to %s bits.", endBitPos)
    val savedLimit = pstate.bitLimit0b
    val postState1 = pstate.withEndBitLimit(endBitPos)
    val postState2 = eParser.parse1(postState1, erd)

    log(LogLevel.Debug, "Restoring data limit to %s bits.", pstate.bitLimit0b)

    val postState3 = postState2.withEndBitLimit(savedLimit)
    val finalState = postState3.status match {
      case Success => {
        // Check that the parsed length is less than or equal to the length of the parent
        //Assert.invariant(postState2.bitPos <= endBitPos)
        this.PECheck(postState2.bitPos <= endBitPos, "The parsed length of the children (%s bits) was greater than that of the parent (%s bits).", postState2.bitPos, endBitPos)
        postState3.withPos(endBitPos, -1, Nope)
      }
      case _ => postState3
    }
    finalState
  }

}

class SpecifiedLengthPatternParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  charset: Charset,
  pattern: String,
  knownEncodingStringBitLengthFunction: String => Int)
  extends SpecifiedLengthParserBase(eParser, erd) {

  val d = new ThreadLocal[DFDLDelimParser] {
    override def initialValue() = {
      new DFDLDelimParser(knownEncodingStringBitLengthFunction)
    }
  }

  def parse(start: PState): PState = withParseErrorThrowing(start) {
    val in = start.inStream

    val reader = in.getCharReader(charset, start.bitPos)

    val result = d.get.parseInputPatterned(pattern, reader, start)

    val endBitPos =
      result match {
        case _: DelimParseFailure => start.bitPos + 0 // no match == length is zero!
        case s: DelimParseSuccess => start.bitPos + s.numBits
      }
    val postEState = parse(start, endBitPos)
    postEState
  }
}

class SpecifiedLengthExplicitBitsParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  charset: Charset,
  length: CompiledExpression,
  toBits: Int)
  extends SpecifiedLengthParserBase(eParser, erd) {

  // TODO: These SpecifiedLength* classes need some refactorization. This
  // function and getLength are all copied in numerous places 

  def getBitLength(s: PState): (PState, Long) = {
    val R(nBytesAsAny, newVMap) = length.evaluate(s.parentElement, s.variableMap, s)
    val nBytes = nBytesAsAny.asInstanceOf[Long]
    val start = s.withVariables(newVMap)

    (start, nBytes * toBits)
  }

  def parse(start: PState): PState = withParseErrorThrowing(start) {

    val (pState, nBits) = getBitLength(start)
    val in = pState.inStream

    try {
      val nBytes = scala.math.ceil(nBits / 8.0).toLong
      val bytes = in.getBytes(pState.bitPos, nBytes)
      val endBitPos = pState.bitPos + nBits
      val postEState = parse(pState, endBitPos)
      return postEState
    } catch {
      case ex: IndexOutOfBoundsException => {
        // Insufficient bytes in field, but we need to still allow processing
        // to test for Nils
        val endBitPos = start.bitPos + 0
        val postEState = parse(start, endBitPos)
        return postEState
      }
      case u: UnsuppressableException => throw u
      case e: Exception => { return PE(pState, "SpecifiedLengthExplicitBitsParser - Exception: \n%s", e.getStackTraceString) }
    }
  }
}

class SpecifiedLengthExplicitBitsFixedParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  nBits: Long,
  charset: Charset)
  extends SpecifiedLengthParserBase(eParser, erd) {

  def parse(start: PState): PState = withParseErrorThrowing(start) {

    val in = start.inStream

    try {
      val nBytes = scala.math.ceil(nBits / 8.0).toLong
      val bytes = in.getBytes(start.bitPos, nBytes)
      val endBitPos = start.bitPos + nBits
      val postEState = parse(start, endBitPos)
      return postEState
    } catch {
      case ex: IndexOutOfBoundsException => {
        // Insufficient bits in field, but we need to still allow processing
        // to test for Nils
        val endBitPos = start.bitPos + 0
        val postEState = parse(start, endBitPos)
        return postEState
      }
      case u: UnsuppressableException => throw u
      case ex: Exception => { return PE(start, "SpecifiedLengthExplicitBitsFixedParser - Exception: \n%s", ex.getStackTraceString) }
    }
  }
}

class SpecifiedLengthExplicitBytesParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  charset: Charset,
  length: CompiledExpression)
  extends SpecifiedLengthParserBase(eParser, erd) {

  def getLength(s: PState): (PState, Long) = {
    val R(nBytesAsAny, newVMap) = length.evaluate(s.parentElement, s.variableMap, s)
    val nBytes = nBytesAsAny.asInstanceOf[Long]
    val start = s.withVariables(newVMap)
    (start, nBytes)
  }

  def parse(start: PState): PState = withParseErrorThrowing(start) {

    val (pState, nBytes) = getLength(start)
    val in = pState.inStream

    try {
      val bytes = in.getBytes(pState.bitPos, nBytes)
      val endBitPos = pState.bitPos + (nBytes * 8)
      val postEState = parse(pState, endBitPos)
      return postEState
    } catch {
      case ex: IndexOutOfBoundsException => {
        // Insufficient bytes in field, but we need to still allow processing
        // to test for Nils
        val endBitPos = start.bitPos + 0
        val postEState = parse(start, endBitPos)
        return postEState
      }
      case u: UnsuppressableException => throw u
      case ex: Exception => { return PE(pState, "SpecifiedLengthExplicitBytesParser - Exception: \n%s", ex.getStackTraceString) }
    }
  }
}

class SpecifiedLengthExplicitBytesFixedParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  nBytes: Long,
  charset: Charset)
  extends SpecifiedLengthParserBase(eParser, erd) {

  def parse(start: PState): PState = withParseErrorThrowing(start) {

    val in = start.inStream

    try {
      // val bytes = in.getBytes(start.bitPos, nBytes)
      val endBitPos = start.bitPos + (nBytes * 8)
      val postEState = super.parse(start, endBitPos)
      return postEState
    } catch {
      case ex: IndexOutOfBoundsException => {
        // Insufficient bytes in field, but we need to still allow processing
        // to test for Nils
        val endBitPos = start.bitPos + 0
        val postEState = super.parse(start, endBitPos)
        return postEState
      }
      case u: UnsuppressableException => throw u
    }
  }
}

class SpecifiedLengthExplicitCharactersFixedParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  nChars: Long,
  charset: Charset,
  knownEncodingStringBitLengthFunction: String => Int)
  extends SpecifiedLengthParserBase(eParser, erd) {

  def parse(start: PState): PState = withParseErrorThrowing(start) {

    val in = start.inStream
    val rdr = in.getCharReader(charset, start.bitPos)
    val field = rdr.getStringInChars(nChars.toInt).toString() // TODO: Don't we want getStringInChars to accept Long?!
    val fieldLength = field.length
    val endBitPos =
      if (fieldLength != nChars.toInt) start.bitPos + 0 // no match == length is zero!
      else {
        val numBits = knownEncodingStringBitLengthFunction(field)
        start.bitPos + numBits
      }
    val postEState = parse(start, endBitPos)
    postEState
  }
}

class SpecifiedLengthExplicitCharactersParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  charset: Charset,
  length: CompiledExpression,
  knownEncodingStringBitLengthFunction: String => Int)
  extends SpecifiedLengthParserBase(eParser, erd) {

  def getLength(s: PState): (PState, Long) = {
    val R(nBytesAsAny, newVMap) = length.evaluate(s.parentElement, s.variableMap, s)
    val nBytes = nBytesAsAny.asInstanceOf[Long]
    val start = s.withVariables(newVMap)
    (start, nBytes)
  }

  def parse(start: PState): PState = withParseErrorThrowing(start) {

    val (pState, nChars) = getLength(start)
    val in = pState.inStream
    val rdr = in.getCharReader(charset, pState.bitPos)

    val field = rdr.getStringInChars(nChars.toInt).toString()
    val fieldLength = field.length
    val endBitPos =
      if (fieldLength != nChars.toInt) pState.bitPos + 0 // no match == length is zero!
      else {
        val numBits = knownEncodingStringBitLengthFunction(field)
        pState.bitPos + numBits
      }

    val postEState = parse(pState, endBitPos)
    postEState
  }
}

