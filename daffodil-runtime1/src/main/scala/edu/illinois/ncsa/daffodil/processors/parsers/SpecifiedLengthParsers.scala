package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.dpath.AsIntConverters
import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.WithParseErrorThrowing
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.Success
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.processors.ScalaPatternParser

abstract class SpecifiedLengthParserBase(eParser: DaffodilParser,
  erd: ElementRuntimeData)
  extends DaffodilParser(erd)
  with WithParseErrorThrowing {

  override lazy val childProcessors = Seq(eParser)

  final def parse(pstate: PState, endBitPos: Long) = {
    log(LogLevel.Debug, "Limiting data to %s bits.", endBitPos)
    val savedLimit = pstate.bitLimit0b
    val startPos0b = pstate.bitPos0b
    pstate.setEndBitLimit(endBitPos)
    eParser.parse1(pstate, erd)
    val limitedLength = endBitPos - startPos0b
    val endOfChildrenPos0b = pstate.bitPos0b
    val childrenLength = endOfChildrenPos0b - startPos0b

    log(LogLevel.Debug, "Restoring data limit to %s bits.", pstate.bitLimit0b)

    pstate.setEndBitLimit(savedLimit)
    pstate.status match {
      case Success => {
        // Check that the parsed length is less than or equal to the length of the parent
        //Assert.invariant(postState2.bitPos <= endBitPos)
        this.PECheck(pstate.bitPos <= endBitPos, "The parsed length of the children (%s bits) was greater than that of the parent (%s bits).", childrenLength, limitedLength)
        pstate.setPos(endBitPos, -1, Nope)
      }
      case _ => //ok
    }
  }

}

class SpecifiedLengthPatternParser(
  eParser: DaffodilParser,
  erd: ElementRuntimeData,
  patternString: String)
  extends SpecifiedLengthParserBase(eParser, erd) {

  private lazy val compiledPattern = ScalaPatternParser.compilePattern(patternString, erd)

  def parse(start: PState): Unit = withParseErrorThrowing(start) {
    val in = start.inStream

    val reader = in.getCharReader(erd.encodingInfo.knownEncodingCharset.charset, start.bitPos)

    val result = ScalaPatternParser.parseInputPatterned(compiledPattern, reader)

    val endBitPos =
      result match {
        case f if f.isFailure => start.bitPos + 0 // no match == length is zero!
        case s => start.bitPos + s.numBits(erd)
      }
    parse(start, endBitPos)
  }
}

class SpecifiedLengthExplicitBitsParser(
  eParser: DaffodilParser,
  erd: ElementRuntimeData,
  dcharset: DFDLCharset,
  length: CompiledExpression,
  toBits: Int)
  extends SpecifiedLengthParserBase(eParser, erd) {

  // TODO: These SpecifiedLength* classes need some refactorization. This
  // function and getLength are all copied in numerous places 

  def getBitLength(s: PState): Long = {
    val nBytesAsAny = length.evaluate(s)
    val nBytes = AsIntConverters.asLong(nBytesAsAny)
    nBytes * toBits
  }

  def parse(pState: PState): Unit = withParseErrorThrowing(pState) {

    val nBits = getBitLength(pState)
    val in = pState.inStream

    val startBitPos = pState.bitPos
    try {
      val nBytes = scala.math.ceil(nBits / 8.0).toLong
      val bytes = in.getBytes(pState.bitPos, nBytes)
      val endBitPos = pState.bitPos + nBits
      parse(pState, endBitPos)
    } catch {
      case ex: IndexOutOfBoundsException => {
        // Insufficient bytes in field, but we need to still allow processing
        // to test for Nils
        val endBitPos = startBitPos + 0
        parse(pState, endBitPos)
      }
      case u: UnsuppressableException => throw u
      case e: Exception => {
        PE(pState, "SpecifiedLengthExplicitBitsParser - Exception: \n%s", e.getStackTraceString)
      }
    }
  }
}

class SpecifiedLengthExplicitBitsFixedParser(
  eParser: DaffodilParser,
  erd: ElementRuntimeData,
  nBits: Long,
  dcharset: DFDLCharset)
  extends SpecifiedLengthParserBase(eParser, erd) {

  def parse(start: PState): Unit = withParseErrorThrowing(start) {

    val in = start.inStream

    try {
      val nBytes = scala.math.ceil(nBits / 8.0).toLong
      val bytes = in.getBytes(start.bitPos, nBytes)
      val endBitPos = start.bitPos + nBits
      parse(start, endBitPos)
    } catch {
      case ex: IndexOutOfBoundsException => {
        // Insufficient bits in field, but we need to still allow processing
        // to test for Nils
        val endBitPos = start.bitPos + 0
        parse(start, endBitPos)
      }
      case u: UnsuppressableException => throw u
      case ex: Exception => { PE(start, "SpecifiedLengthExplicitBitsFixedParser - Exception: \n%s", ex.getStackTraceString) }
    }
  }
}

class SpecifiedLengthExplicitBytesParser(
  eParser: DaffodilParser,
  erd: ElementRuntimeData,
  dcharset: DFDLCharset,
  length: CompiledExpression)
  extends SpecifiedLengthParserBase(eParser, erd) {

  def getLength(s: PState): Long = {
    val nBytesAsAny = length.evaluate(s)
    val nBytes = AsIntConverters.asLong(nBytesAsAny)
    nBytes
  }

  def parse(pState: PState): Unit = withParseErrorThrowing(pState) {
    val limit = pState.bitLimit0b
    val lenAvailable = pState.bitLimit0b - pState.bitPos0b
    val nBytes = getLength(pState)
    val in = pState.inStream
    val bytes = try {
      in.getBytes(pState.bitPos, nBytes)
    } catch {
      case ex: IndexOutOfBoundsException => {
        PE("Insufficient data. Required %s bytes.%s", nBytes,
          {

            if (limit != -1) " Only %s bytes were available.".format(scala.math.ceil(lenAvailable / 8.0).toLong)
            else ""
          })
        // Insufficient bytes in field, but we need to still allow processing
        // to test for Nils
        //        val endBitPos = start.bitPos + 0
        //        val postEState = parse(start, endBitPos)
        //        return postEState
      }
    }
    val endBitPos = pState.bitPos + (nBytes * 8)
    parse(pState, endBitPos)
  }
}

class SpecifiedLengthExplicitBytesFixedParser(
  eParser: DaffodilParser,
  erd: ElementRuntimeData,
  nBytes: Long,
  dcharset: DFDLCharset)
  extends SpecifiedLengthParserBase(eParser, erd) {

  def parse(start: PState): Unit = withParseErrorThrowing(start) {

    val in = start.inStream

    try {
      // val bytes = in.getBytes(start.bitPos, nBytes)
      val endBitPos = start.bitPos + (nBytes * 8)
      super.parse(start, endBitPos)
    } catch {
      case ex: IndexOutOfBoundsException => {
        // Insufficient bytes in field, but we need to still allow processing
        // to test for Nils
        val endBitPos = start.bitPos + 0
        super.parse(start, endBitPos)
      }
      case u: UnsuppressableException => throw u
    }
  }
}

class SpecifiedLengthExplicitCharactersFixedParser(
  eParser: DaffodilParser,
  erd: ElementRuntimeData,
  nChars: Long)
  extends SpecifiedLengthParserBase(eParser, erd) {

  def parse(start: PState): Unit = withParseErrorThrowing(start) {

    val in = start.inStream
    val rdr = in.getCharReader(erd.encodingInfo.knownEncodingCharset.charset, start.bitPos)
    val field = rdr.getStringInChars(nChars.toInt).toString() // TODO: Don't we want getStringInChars to accept Long?!
    val fieldLength = field.length
    val endBitPos =
      if (fieldLength != nChars.toInt) start.bitPos + 0 // no match == length is zero!
      else {
        val numBits = erd.encodingInfo.knownEncodingStringBitLength(field)
        start.bitPos + numBits
      }
    parse(start, endBitPos)
  }
}

class SpecifiedLengthExplicitCharactersParser(
  eParser: DaffodilParser,
  erd: ElementRuntimeData,
  length: CompiledExpression)
  extends SpecifiedLengthParserBase(eParser, erd) {

  def getLength(s: PState): Long = {
    val nBytesAsAny = length.evaluate(s)
    val nBytes = AsIntConverters.asLong(nBytesAsAny)
    nBytes
  }

  def parse(pState: PState): Unit = withParseErrorThrowing(pState) {

    val nChars = getLength(pState)
    val in = pState.inStream
    val rdr = in.getCharReader(erd.encodingInfo.knownEncodingCharset.charset, pState.bitPos)

    val field = rdr.getStringInChars(nChars.toInt).toString()
    val fieldLength = field.length
    val endBitPos =
      if (fieldLength != nChars.toInt) pState.bitPos + 0 // no match == length is zero!
      else {
        val numBits = erd.encodingInfo.knownEncodingStringBitLength(field)
        pState.bitPos + numBits
      }

    parse(pState, endBitPos)
  }
}
