package daffodil.processors

import java.math.BigInteger
import java.text.{ ParseException, ParsePosition }
import java.util.regex.Pattern
import java.nio.{ CharBuffer, ByteBuffer }
import java.nio.charset.{ CharsetEncoder, CharsetDecoder }
import scala.collection.mutable.Queue
import daffodil.dsom._
import daffodil.compiler._
import daffodil.xml.XMLUtils
import daffodil.schema.annotation.props.gen.{ YesNo, LengthKind, ByteOrder }
import daffodil.util.{ Debug, LogLevel, Logging, Info }
import daffodil.util.Misc.bytes2Hex
import daffodil.processors._
import daffodil.exceptions.Assert
import stringsearch.constructs.{ EscapeScheme, SearchResult }
import stringsearch.delimiter.Delimiter
import com.ibm.icu.text.{ NumberFormat, DecimalFormat }
import daffodil.grammar.Terminal

case class ElementBegin(e: ElementBase) extends Terminal(e, e.isComplexType != true || e.lengthKind != LengthKind.Pattern) {

  def parser: Parser = new Parser(e) {
    override def toString = "<" + e.name + ">"

    /**
     * ElementBegin just adds the element we are constructing to the infoset and changes
     * the state to be referring to this new element as what we're parsing data into.
     */
    def parse(start: PState): PState = {
      val currentElement = new org.jdom.Element(e.name, e.targetNamespacePrefix, e.targetNamespace)
      log(Debug("currentElement = %s", currentElement))
      val priorElement = start.parentForAddContent
      priorElement.addContent(currentElement)
      log(Debug("priorElement = %s", priorElement))
      val postState = start.withParent(currentElement)
      postState
    }
  }

  def unparser: Unparser = new Unparser(e) {
    override def toString = "<" + e.name + ">"

    /**
     * Changes the state to refer to the next element in the infoset as the element to unparse.
     */
    def unparse(start: UState): UState = {
      val nextElement = {
        //
        // TODO FIXME: THis can't be correct. The elementBegin shouldn't be writing out element contents.
        // That should happen in content unparsers. This unparser should just set things up so content
        // unparsers see the correct element to take the contents of. Which really means just changing the 
        // parent pointer in the UState.
        //
        // TODO: should not try/catch - should return a failed UState on error
        try { //if content contains elements
          if (!start.childIndexStack.isEmpty) {
            if (start.childPos != 1) { //if not first child, write unparsed result of previous child to outputStream
              //              val encoder = e.knownEncodingEncoder
              //              start.outStream.setEncoder(encoder)
              start.outStream.write()
            }
            start.currentElement.getContent().get(start.childPos.asInstanceOf[Int] - 1).asInstanceOf[org.jdom.Element]
          } else {
            //            val encoder = e.knownEncodingEncoder
            //            start.outStream.setEncoder(encoder)
            start.currentElement.getContent().get(0).asInstanceOf[org.jdom.Element]
          }
        } catch {
          case e: Exception => start.currentElement //if content is text
        }
      }

      start.withCurrent(nextElement).moveOverByOne
    }
  }
}

case class ComplexElementBeginPattern(e: ElementBase)
  extends Terminal(e, e.isComplexType == true && e.lengthKind == LengthKind.Pattern)
  with WithParseErrorThrowing {
  Assert.invariant(e.isComplexType)

  def parser: Parser = new Parser(e) {
    override def toString = "<" + e.name + " dfdl:lengthKind='pattern'>"
    val decoder = e.knownEncodingDecoder
    var cbuf = CharBuffer.allocate(1024) // TODO: Performance: get a char buffer from a pool.
    val pattern = e.lengthPattern

    /**
     * ElementBegin just adds the element we are constructing to the infoset and changes
     * the state to be referring to this new element as what we're parsing data into.
     */
    def parse(start: PState): PState = withParseErrorThrowing(start) {
      withLoggingLevel(LogLevel.Info) {
        val eName = e.toString()

        log(Debug("ComplexElementBeginPattern - " + eName + " - Parsing pattern at byte position: " + (start.bitPos >> 3)))
        log(Debug("ComplexElementBeginPattern - " + eName + " - Parsing pattern at bit position: " + start.bitPos))

        val in: InStreamFromByteChannel = start.inStream.asInstanceOf[InStreamFromByteChannel]

        val bytePos = (start.bitPos >> 3).toInt

        val byteReader = in.byteReader.atPos(bytePos)
        val reader = byteReader.charReader(decoder.charset().name())

        val d = new delimsearch.DelimParser()

        var result: delimsearch.DelimParseResult = new delimsearch.DelimParseResult

        result = d.parseInputPatterned(pattern, reader, decoder.charset())

        val postState1 = result.isSuccess match {
          case true => {
            val endBitPos = start.bitPos + (result.numBytes * 8)
            log(Debug("Parsed: " + result.field))
            log(Debug("Ended at bit position " + endBitPos))
            val limitedInStream = in.withLimit(start.bitPos, endBitPos)
            // Since we've created a new sub-stream with just the limited part of data in it,
            // don't forget to have the position in it start at zero.
            start withEndBitLimit (endBitPos) withInStream (new InStreamFromByteChannel(e, limitedInStream)) withPos (0, 0)
          }
          case false => { start.failed(this.toString() + ": No match found!") }
        }
        val currentElement = new org.jdom.Element(e.name, e.targetNamespacePrefix, e.targetNamespace)
        log(Debug("currentElement = %s", currentElement))
        val priorElement = postState1.parentForAddContent
        priorElement.addContent(currentElement)
        log(Debug("priorElement = %s", priorElement))
        val postState2 = postState1 withParent (currentElement)
        postState2
      }
    }
  }

  def unparser: Unparser = new Unparser(e) {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

abstract class ElementEndBase(e: ElementBase) extends Terminal(e, e.isComplexType != true || e.lengthKind != LengthKind.Pattern) {
  def toPrettyString = "</" + e.name + prettyStringModifier + ">"
  def prettyStringModifier: String

  def move(pstate: PState): PState // implement for different kinds of "moving over to next thing"

  def parser: Parser = new Parser(e) {
    override def toString = toPrettyString

    /**
     * ElementEnd just moves back to the parent element of the current one.
     */
    def parse(start: PState): PState = {
      val currentElement = start.parent
      // Assert.invariant(currentElement.getName() != "_document_" )
      val priorElement = currentElement.getParent
      log(Debug("priorElement = %s", priorElement))
      val postState = move(start.withParent(priorElement))
      postState
    }
  }

  def unparser: Unparser = new Unparser(e) {
    override def toString = "</" + e.name + ">"

    /**
     * Changes state to refer to parent element of the current one.
     */
    def unparse(start: UState): UState = {
      val postState = {
        if (start.currentElement.getName != start.rootName) {
          val parent = start.currentElement.getParentElement()
          val state = start.withCurrent(parent)
          state
        } else {
          start
        }
      }
      postState
    }
  }
}

case class ElementEnd(e: ElementBase) extends ElementEndBase(e) {
  def move(pstate: PState) = pstate.moveOverByOne
  def prettyStringModifier = ""
}

case class ElementEndNoRep(e: ElementBase) extends ElementEndBase(e) {
  def move(pstate: PState) = pstate.moveOverOneElementChildOnly
  def prettyStringModifier = "(NoRep)"
}

case class ComplexElementEndPattern(e: ElementBase) extends Terminal(e, e.isComplexType == true && e.lengthKind == LengthKind.Pattern) {
  // TODO: Should this be more generic; is there a way to detect state from the current element to tell us if it's time
  //       to pop the input stack?

  def parser: Parser = new Parser(e) {
    override def toString = "</" + e.name + " dfdl:lengthKind='pattern'>"

    /**
     * ElementEnd just moves back to the parent element of the current one.
     */
    def parse(start: PState): PState = {
      val currentElement = start.parent
      log(Debug("currentElement = %s", currentElement))
      // Assert.invariant(currentElement.getName() != "_document_" )
      //val priorElement = currentElement.getParent.asInstanceOf[org.jdom.Element]
      var priorElement = currentElement.getParent
      log(Debug("priorElement = %s", priorElement))
      val postState = start.withParent(priorElement).moveOverByOne.withLastInStream()
      postState
    }
  }

  def unparser: Unparser = new Unparser(e) {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

/**
 * The I/O layer should be written to use Java's NIO Channels, and Direct ByteBuffers for file I/O. This is the
 * fastest stuff in the Java stack.
 *
 * The basic design for known-length parsing
 * is to do a bounds check on whether we have enough buffer space to accomplish the reading of data
 * within the buffer. If not, repositioning/refreshing the buffer. If so, then we want to issue a
 * single block read per element which both reads and converts to the right character encoding, or
 * directly to the right type (float, long, double, etc) when the result type is a primitive.
 *
 * For types with more complexity, (binary dates for example) then the previous would still be done
 * followed by a conversion of some sort.
 */

case class StringFixedLengthInBytes(e: ElementBase, nBytes: Long)
  extends Terminal(e, true)
  with WithParseErrorThrowing {

  def parser: Parser = new Parser(e) {
    override def toString = "StringFixedLengthInBytesParser(" + nBytes + ")"
    val decoder = e.knownEncodingDecoder

    def parse(start: PState): PState = withParseErrorThrowing(start) {
      withLoggingLevel(LogLevel.Info) {
        log(Debug("StringFixedLengthInBytes - Parsing starting at bit position: " + start.bitPos))

        val in: InStreamFromByteChannel = start.inStream.asInstanceOf[InStreamFromByteChannel]

        val bytePos = (start.bitPos >> 3).toInt

        try {
          val bytes = in.getBytes(start.bitPos, nBytes.toInt)
          val cb = decoder.decode(ByteBuffer.wrap(bytes))
          val result = cb.toString
          val endBitPos = start.bitPos + (nBytes.toInt * 8)
          log(Debug("Parsed: " + result))
          log(Debug("Ended at bit position " + endBitPos))
          val endCharPos = start.charPos + result.length
          val currentElement = start.parentForAddContent
          // Assert.invariant(currentElement.getName != "_document_")
          // Note: this side effect is backtracked, because at points of uncertainty, pre-copies of a node are made
          // and when backtracking occurs they are used to replace the nodes modified by sub-parsers.
          currentElement.addContent(new org.jdom.Text(result))
          val postState = start.withPos(endBitPos, endCharPos)
          return postState
        } catch {
          case e: java.nio.BufferUnderflowException => { return PE(start, "StringFixedLengthInBytes - Insufficient Bits in field; required " + nBytes * 8) }
          case e: IndexOutOfBoundsException => { return PE(start, "StringFixedLengthInBytes - IndexOutOfBounds: " + e.getMessage()) }
          case e: Exception => { return start.failed("StringFixedLengthInBytes - Exception: " + e.getStackTraceString) }
        }
      }
    }
  }

  def unparser: Unparser = new Unparser(e) {
    override def toString = "StringFixedLengthInBytesUnparser(" + nBytes + ")"
    val encoder = e.knownEncodingEncoder

    def unparse(start: UState): UState = {
      // setLoggingLevel(LogLevel.Debug)

      val data = start.currentElement.getText

      start.outStream.setEncoder(encoder)
      start.outStream.fillCharBuffer(data)

      log(Debug("Unparsed: " + start.outStream.getData))
      start
    }
  }
}

case class StringFixedLengthInBytesVariableWidthCharacters(e: ElementBase, nBytes: Long)
  extends Terminal(e, true)
  with WithParseErrorThrowing {

  def parser: Parser = new Parser(e) {
    override def toString = "StringFixedLengthInBytesVariableWidthCharactersParser(" + nBytes + ")"
    val decoder = e.knownEncodingDecoder

    def parse(start: PState): PState = withParseErrorThrowing(start) {
      withLoggingLevel(LogLevel.Info) {
        log(Debug("Parsing starting at bit position: " + start.bitPos))

        val in: InStreamFromByteChannel = start.inStream.asInstanceOf[InStreamFromByteChannel]

        val bytePos = (start.bitPos >> 3).toInt

        val byteReader = in.byteReader.atPos(bytePos)

        try {
          val bytes = in.getBytes(start.bitPos, nBytes.toInt)
          val cb = decoder.decode(ByteBuffer.wrap(bytes))
          val result = cb.toString
          val endBitPos = start.bitPos + (nBytes.toInt * 8)
          log(Debug("Parsed: " + result))
          log(Debug("Ended at bit position " + endBitPos))
          val endCharPos = start.charPos + result.length
          val currentElement = start.parentForAddContent
          // Assert.invariant(currentElement.getName != "_document_")
          // Note: this side effect is backtracked, because at points of uncertainty, pre-copies of a node are made
          // and when backtracking occurs they are used to replace the nodes modified by sub-parsers.
          currentElement.addContent(new org.jdom.Text(result))
          val postState = start.withPos(endBitPos, endCharPos)
          return postState
        } catch {
          case e: java.nio.BufferUnderflowException => { return PE(start, "StringFixedLengthInBytesVariableWidthCharacters - Insufficient Bits in field; required " + nBytes * 8) }
          case e: IndexOutOfBoundsException => { return PE(start, "StringFixedLengthInBytesVariableWidthCharacters - " + e.getMessage()) }
          case e: Exception => { return PE(start, "StringFixedLengthInBytesVariableWidthCharacters - " + e.getMessage()) }
        }
        //      // setLoggingLevel(LogLevel.Debug)
        //
        //      log(Debug(this.toString() + " - Parsing starting at bit position: " + start.bitPos))
        //
        //      // We know the nBytes, decode only until we've reached this value.
        //      val in = start.inStream
        //      val (endBitPos, _) = in.fillCharBufferMixedData(cbuf, start.bitPos, decoder, nBytes)
        //      if (endBitPos < start.bitPos + nBytes * 8) {
        //        return PE(start, "Insufficent Bits in field; required " + nBytes * 8 + " received " + (endBitPos - start.bitPos))
        //      }
        //      val result = cbuf.toString
        //
        //      if (result == null) { return start.failed(this.toString() + " - Result was null!") }
        //
        //      val resultBytes = result.getBytes(decoder.charset())
        //
        //      if (resultBytes.length < nBytes) { return start.failed(this.toString() + " - Result(" + resultBytes.length + ") was not at least nBytes (" + nBytes + ") long.") }
        //
        //      log(Debug("Parsed: " + result))
        //      log(Debug("Ended at bit position " + endBitPos))
        //      val endCharPos = start.charPos + result.length
        //      val currentElement = start.parentForAddContent
        //      // Assert.invariant(currentElement.getName != "_document_")
        //      // Note: this side effect is backtracked, because at points of uncertainty, pre-copies of a node are made
        //      // and when backtracking occurs they are used to replace the nodes modified by sub-parsers.
        //      currentElement.addContent(new org.jdom.Text(result))
        //      val postState = start.withPos(endBitPos, endCharPos)
        //      postState
        start
      }
    }
  }

  def unparser: Unparser = new Unparser(e) {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

case class StringFixedLengthInVariableWidthCharacters(e: ElementBase, nChars: Long)
  extends Terminal(e, true)
  with WithParseErrorThrowing {

  def parser: Parser = new Parser(e) {
    override def toString = "StringFixedLengthInVariableWidthCharactersParser(" + nChars + ")"
    val decoder = e.knownEncodingDecoder

    def parse(start: PState): PState = withParseErrorThrowing(start) {
      withLoggingLevel(LogLevel.Info) {
        log(Debug("Parsing starting at bit position: " + start.bitPos))

        val in: InStreamFromByteChannel = start.inStream.asInstanceOf[InStreamFromByteChannel]

        val bytePos = (start.bitPos >> 3).toInt

        val byteReader = in.byteReader.atPos(bytePos)

        val reader = byteReader.charReader(decoder.charset().name())

        val d = new delimsearch.DelimParser()

        val result = d.parseInputNCharacters(nChars, reader, decoder.charset())

        if (!result.isSuccess) {
          return PE(start, "Parse failed to find exactly " + nChars + " characters.")
        }

        val parsedField = result.field
        val parsedBytes = result.numBytes
        val endBitPos = start.bitPos + (parsedBytes * 8)

        log(Debug("Parsed: " + parsedField))
        log(Debug("Ended at bit position: " + endBitPos))

        val endCharPos = start.charPos + nChars
        val currentElement = start.parentForAddContent
        currentElement.addContent(new org.jdom.Text(parsedField))
        val postState = start.withPos(endBitPos, endCharPos)
        postState

        //      //setLoggingLevel(LogLevel.Debug)
        //
        //      log(Debug(this.toString() + " - Parsing starting at bit position: " + start.bitPos))
        //
        //      // We don't know the width of the characters, so decode as much data as possible.
        //      // We will truncate as necessary later.
        //      val in = start.inStream
        //      val (endBitPos, _) = in.fillCharBufferMixedData(cbuf, start.bitPos, decoder)
        //
        //      val result = cbuf.toString
        //
        //      if (result == null) { return start.failed(this.toString() + " - Result was null!") }
        //
        //      val finalResult = result.substring(0, nChars.toInt) // Truncate
        //      val finalResultBytes = finalResult.getBytes(decoder.charset()).length
        //      val finalBitPos = 8 * finalResultBytes + start.bitPos
        //
        //      if (finalResult.length < nChars) { return start.failed(this.toString() + " - Result(" + finalResult.length + ") was not at least nChars (" + nChars + ") long.") }
        //
        //      log(Debug("Parsed: " + finalResult))
        //      log(Debug("Ended at bit position " + finalBitPos))
        //      val endCharPos = start.charPos + nChars
        //      val currentElement = start.parentForAddContent
        //      // Assert.invariant(currentElement.getName != "_document_")
        //      // Note: this side effect is backtracked, because at points of uncertainty, pre-copies of a node are made
        //      // and when backtracking occurs they are used to replace the nodes modified by sub-parsers.
        //      currentElement.addContent(new org.jdom.Text(finalResult))
        //      val postState = start.withPos(finalBitPos, endCharPos)
        //      postState
      }
    }
  }

  def unparser: Unparser = new Unparser(e) {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

case class StringDelimitedEndOfData(e: ElementBase)
  extends Terminal(e, true)
  with WithParseErrorThrowing {
  lazy val es = e.escapeScheme
  lazy val esObj = EscapeScheme.getEscapeScheme(es, e)
  lazy val tm = e.allTerminatingMarkup
  lazy val cname = toString

  def parser: Parser = new Parser(e) {
    override def toString = cname + "(" + tm.map { _.prettyExpr } + ")"
    val decoder = e.knownEncodingDecoder

    def parse(start: PState): PState = withParseErrorThrowing(start) {
      withLoggingLevel(LogLevel.Info) {

        val eName = e.toString()

        // We must feed variable context out of one evaluation and into the next.
        // So that the resulting variable map has the updated status of all evaluated variables.
        var vars = start.variableMap
        val delimsRaw = e.allTerminatingMarkup.map {
          x =>
            {
              val R(res, newVMap) = x.evaluate(start.parent, vars)
              vars = newVMap
              res
            }
        }
        val delimsCooked1 = delimsRaw.map(raw => { new daffodil.dsom.ListOfStringValueAsLiteral(raw.toString, e).cooked })
        val delimsCooked = delimsCooked1.flatten
        val postEvalState = start.withVariables(vars)

        log(Debug(eName + " - Looking for: " + delimsCooked + " Count: " + delimsCooked.length))
        val in: InStreamFromByteChannel = start.inStream.asInstanceOf[InStreamFromByteChannel]

        val bytePos = (postEvalState.bitPos >> 3).toInt
        log(Debug(eName + " - Starting at bit pos: " + postEvalState.bitPos))
        log(Debug(eName + " - Starting at byte pos: " + bytePos))

        val byteReader = in.byteReader.atPos(bytePos)
        val reader = byteReader.charReader(decoder.charset().name())

        val d = new delimsearch.DelimParser()

        var result: delimsearch.DelimParseResult = new delimsearch.DelimParseResult

        if (esObj.escapeSchemeKind == stringsearch.constructs.EscapeSchemeKind.Block) {
          result = d.parseInputEscapeBlock(Set.empty[String], delimsCooked.toSet, reader,
            esObj.escapeBlockStart, esObj.escapeBlockEnd, esObj.escapeEscapeCharacter, decoder.charset())
        } else if (esObj.escapeSchemeKind == stringsearch.constructs.EscapeSchemeKind.Character) {
          result = d.parseInputEscapeCharacter(Set.empty[String], delimsCooked.toSet, reader,
            esObj.escapeCharacter, esObj.escapeEscapeCharacter, decoder.charset())
        } else {
          result = d.parseInput(Set.empty[String], delimsCooked.toSet, reader, decoder.charset())
        }

        if (!result.isSuccess) {
          return postEvalState.failed(this.toString() + " - " + eName + " - Parse failed.")
        } else {
          val field = result.field
          val numBytes = result.numBytes
          log(Debug(eName + " - Parsed: " + field + " Parsed Bytes: " + numBytes))
          val endCharPos = postEvalState.charPos + field.length()
          val endBitPos = postEvalState.bitPos + (numBytes * 8)
          val currentElement = postEvalState.parentForAddContent
          currentElement.addContent(new org.jdom.Text(field))
          return postEvalState.withPos(endBitPos, endCharPos)
        }
      }
    }
  }

  def unparser: Unparser = new Unparser(e) {
    override def toString = cname + "(" + tm.map { _.prettyExpr } + ")"
    val encoder = e.knownEncodingEncoder

    def unparse(start: UState): UState = {
      // setLoggingLevel(LogLevel.Debug)

      val data = start.currentElement.getText

      start.outStream.setEncoder(encoder)
      start.outStream.fillCharBuffer(data)
      log(Debug("Unparsed: " + start.outStream.getData))
      start
    }
  }
}

case class StringPatternMatched(e: ElementBase)
  extends Terminal(e, true)
  with WithParseErrorThrowing {

  def parser: Parser = new Parser(e) {
    override def toString = "StringPatternMatched"
    val decoder = e.knownEncodingDecoder
    val pattern = e.lengthPattern

    // TODO: Add parameter for changing CharBuffer size

    def parse(start: PState): PState = withParseErrorThrowing(start) {
      withLoggingLevel(LogLevel.Info) {
        val eName = e.toString()

        log(Debug("StringPatternMatched - " + eName + " - Parsing pattern at byte position: " + (start.bitPos >> 3)))
        log(Debug("StringPatternMatched - " + eName + " - Parsing pattern at bit position: " + start.bitPos))

        val in: InStreamFromByteChannel = start.inStream.asInstanceOf[InStreamFromByteChannel]

        val bytePos = (start.bitPos >> 3).toInt

        val byteReader = in.byteReader.atPos(bytePos)
        val reader = byteReader.charReader(decoder.charset().name())

        val d = new delimsearch.DelimParser()

        var result: delimsearch.DelimParseResult = new delimsearch.DelimParseResult

        result = d.parseInputPatterned(pattern, reader, decoder.charset())

        val postState = result.isSuccess match {
          case true => {
            val endBitPos = start.bitPos + (result.numBytes * 8)
            log(Debug("StringPatternMatched - Parsed: " + result.field))
            log(Debug("StringPatternMatched - Ended at bit position " + endBitPos))
            val endCharPos = start.charPos + result.field.length()
            val currentElement = start.parentForAddContent
            currentElement.addContent(new org.jdom.Text(result.field))
            start.withPos(endBitPos, endCharPos)
          }
          case false => { start.failed(this.toString() + ": No match found!") }
        }
        //      log(Debug("Parsing starting at bit position: " + start.bitPos))
        //      val in = start.inStream.asInstanceOf[InStreamFromByteChannel]
        //      var bitOffset = 0L
        //
        //      val (result, endBitPos, theState) = in.fillCharBufferWithPatternMatch(cbuf, start.bitPos, decoder, pattern)
        //
        //      val postState = theState match {
        //        case SearchResult.NoMatch => start.failed(this.toString() + ": No match found!")
        //        case SearchResult.PartialMatch => start.failed(this.toString() + ": Partial match found!")
        //        case SearchResult.FullMatch => {
        //          log(Debug("Parsed: " + result))
        //          log(Debug("Ended at bit position " + endBitPos))
        //          val endCharPos = start.charPos + result.length()
        //          val currentElement = start.parentForAddContent
        //          currentElement.addContent(new org.jdom.Text(result))
        //          start.withPos(endBitPos, endCharPos)
        //        }
        //      }
        //      postState
        postState
      }
    }
  }

  def unparser: Unparser = new Unparser(e) {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

abstract class ConvertTextNumberPrim[S](e: ElementBase, guard: Boolean) extends Terminal(e, guard) {
  protected def getNum(s: Number): S
  protected val GramName = "number"
  protected val GramDescription = "Number"
  override def toString = "to(xs:" + GramName + ")"

  protected def numFormat: NumberFormat
  protected def isInt: Boolean

  protected def isInvalidRange(n: java.lang.Number): Boolean

  def parser: Parser = new Parser(e) {
    override def toString = "to(xs:" + GramName + ")"

    def parse(start: PState): PState = {
      val node = start.parentElement
      var str = node.getText()

      Assert.invariant(str != null) // worst case it should be empty string. But not null.
      val resultState = try {
        // Strip leading + (sign) since the DecimalFormat can't handle it
        if (str.length > 0 && str.charAt(0) == '+') {
          // TODO: There needs to be a way to restore '+' in the unparse, but that will be in the format field
          str = str.substring(1)
        }
        if (str == "") return PE(start, "Convert to %s (for xs:%s): Cannot parse number from empty string", GramDescription, GramName)

        // Need to use Decimal Format to parse even though this is an Integral number
        val df = numFormat
        val pos = new ParsePosition(0)
        val num = try {
          df.parse(str, pos)
        } catch {
          case e: Exception =>
            return PE(start, "Convert to %s (for xs:%s): Parse of '%s' threw exception %s",
              GramDescription, GramName, str, e)

        }
        if (isInvalidRange(num)) {
          return PE(start, "Convert to %s (for xs:%s): Out of Range: '%s' converted to %s, is not in range for the type.",
            GramDescription, GramName, str, num)
        }

        // Verify that what was parsed was what was passed exactly in byte count.  
        // Use pos to verify all characters consumed & check for errors!
        if (pos.getIndex != str.length) {
          return PE(start, "Convert to %s (for xs:%s): Unable to parse '%s' (using up all characters).",
            GramDescription, GramName, str)
        }

        // convert to proper type
        val asNumber = getNum(num)

        // Verify no digits lost (the number was correctly transcribed)
        if (isInt && asNumber.asInstanceOf[Number] != num) {
          // Transcription error
          return PE(start, "Convert to %s (for xs:%s): Invalid data: '%s' parsed into %s, which converted into %s.",
            GramDescription, GramName, str, num, asNumber)
        }

        node.setText(asNumber.toString)

        start
      } // catch { case e: Exception => start.failed("Failed to convert %s to an xs:%s" + GramName) }

      resultState
    }
  }

  // TODO: consolidate duplicate code
  def unparser: Unparser = new Unparser(e) {
    override def toString = "to(xs:" + GramName + ")"

    /*
      * Converts data to number format, returns unparse exception if data cannot be converted to given format.
      */
    def unparse(start: UState): UState = {
      // TODO: OK to get from infoset?
      var str = start.currentElement.getText //gets data from element being unparsed
      Assert.invariant(str != null) // worst case it should be empty string. But not null.
      if (str == "") return UE(start, "Convert to %s (for xs:%s): Cannot unparse number from empty string", GramDescription, GramName)

      //make sure data can parse to appropriate type
      val df = numFormat
      val pos = new ParsePosition(0)
      val num = try {
        df.parse(str, pos)
      } catch {
        case e: Exception =>
          return UE(start, "Convert to %s (for xs:%s): Unparse of '%s' threw exception %s",
            GramDescription, GramName, str, e)
      }
      if (isInvalidRange(num)) {
        return UE(start, "Convert to %s (for xs:%s): Out of Range: '%s' converted to %s, is not in range for the type.",
          GramDescription, GramName, str, num)
      }

      // Verify that what was unparsed was what was passed exactly in byte count.  
      // Use pos to verify all characters consumed & check for errors!
      if (pos.getIndex != str.length) {
        return UE(start, "Convert to %s (for xs:%s): Unable to unparse '%s' (using up all characters).",
          GramDescription, GramName, str)
      }

      // convert to proper type
      val asNumber = getNum(num)

      // Verify no digits lost (the number was correctly transcribed)
      if (isInt && asNumber.asInstanceOf[Number] != num) {
        // Transcription error
        return UE(start, "Convert to %s (for xs:%s): Invalid data: '%s' unparsed into %s, which converted into %s.",
          GramDescription, GramName, str, num, asNumber)
      }

      // TODO: Restore leading '+' sign and leading/trailing 0's, etc. (Need to overwrite number with old formatting in CharBuffer
      //      log(Debug("Adding text number " + asNumber.toString))

      start
    }
  }
}

abstract class ConvertTextIntegerNumberPrim[T](e: ElementBase, g: Boolean)
  extends ConvertTextNumberPrim[T](e, g) {
  override def numFormat = NumberFormat.getIntegerInstance()
  override def isInt = true

  protected def isInvalidRange(n : java.lang.Number) : Boolean = {
    //
    // Note: Scala has no class analogous to java.lang.Number. There's no common 
    // base class above its number types (as there isn't above the Java *primitive* number types.)
    //
    // We're being handed here a java 'boxed' number type, and those have common parent Number.
    //
    // println("number's actual type is: " + n.getClass.getName)
    //
    // This method only for things that fit in range of a Long. (i.e., not unbounded size Integer, and not unsignedLong
    // Nevertheless, if invalid data much too long for the real numeric type is what is found in the data
    // then a java BigInteger (or maybe even BigDecimal might get passed here.
    // 
    // The only thing we can check is whether there is conversion to a long available.
    // e.g., like this: Assert.invariant(n.isInstanceOf[{ def longValue : Long}]) 
    // But that's eliminated by erasure, so we'll just do without.
    //
    if (n == null) false // we tolerate null here. Something else figures out the error.
    else {
      val l = n.longValue
      isInvalidRange(l)
    }
  }
  def min: Long
  def max: Long
  private def isInvalidRange(l: Long) = {
    l < min || l > max
  }
}

abstract class ConvertTextFloatingPointNumberPrim[T](e: ElementBase, g: Boolean)
  extends ConvertTextNumberPrim[T](e, g) {
  override def numFormat = NumberFormat.getNumberInstance() // .getScientificInstance() Note: scientific doesn't allow commas as grouping separators.
  override def isInt = false
}

case class ConvertTextIntegerPrim(e: ElementBase) extends ConvertTextIntegerNumberPrim[BigInteger](e, true) {
  protected override def getNum(num: Number) = new BigInteger(num.toString)
  protected override val GramName = "integer"
  protected override val GramDescription = "Unbounded Integer"
  protected override def isInvalidRange(n: java.lang.Number): Boolean = false
  def min = -1 // ignored
  def max = -1 // ignored
}

case class ConvertTextLongPrim(e: ElementBase) extends ConvertTextIntegerNumberPrim[Long](e, true) {
  protected override def getNum(num: Number) = num.longValue
  protected override val GramName = "long"
  protected override val GramDescription = "Long Integer"
  val min = Long.MinValue
  val max = Long.MaxValue
}

case class ConvertTextIntPrim(e: ElementBase) extends ConvertTextIntegerNumberPrim[Int](e, true) {
  protected override def getNum(num: Number) = num.intValue
  protected override val GramName = "int"
  protected override val GramDescription = "Integer"
  val min = Int.MinValue.toLong
  val max = Int.MaxValue.toLong
}

case class ConvertTextShortPrim(e: ElementBase) extends ConvertTextIntegerNumberPrim[Short](e, true) {
  protected override def getNum(num: Number) = num.shortValue
  protected override val GramName = "short"
  protected override val GramDescription = "Short Integer"
  val min = Short.MinValue.toLong
  val max = Short.MaxValue.toLong
}

case class ConvertTextBytePrim(e: ElementBase) extends ConvertTextIntegerNumberPrim[Byte](e, true) {
  protected override def getNum(num: Number) = num.byteValue
  protected override val GramName = "byte"
  protected override val GramDescription = "Byte"
  val min = Byte.MinValue.toLong
  val max = Byte.MaxValue.toLong
}

case class ConvertTextUnsignedLongPrim(e: ElementBase) extends ConvertTextIntegerNumberPrim[BigInteger](e, true) {
  protected override def getNum(num: Number) = new BigInteger(num.toString)
  protected override val GramName = "unsignedLong"
  protected override val GramDescription = "Unsigned Long"
  protected override def isInvalidRange(jn: java.lang.Number) = {
    jn match {
      case n: BigInteger => {
        n.compareTo(BigInteger.ZERO) < 0 || n.compareTo(BigInteger.ONE.shiftLeft(64)) >= 0
      }
      case null => false // tolerate null. Deal with that error elsewhere.
      case _ => {
        val n = jn.longValue()
        n < 0 // note: the other side of the check is inherently ok since a Long must be smaller than an unsignedLong.
      }
    }
  }
  val min = 0.toLong
  val max = -1.toLong // unused.
}

case class ConvertTextUnsignedIntPrim(e: ElementBase) extends ConvertTextIntegerNumberPrim[Long](e, true) {
  protected override def getNum(num: Number) = num.longValue
  protected override val GramName = "unsignedInt"
  protected override val GramDescription = "Unsigned Int"
  val min = 0L
  val max = (1L << 32) - 1L
}
// TODO: Restore leading '+' sign and leading/trailing 0's
case class ConvertTextUnsignedShortPrim(e: ElementBase) extends ConvertTextIntegerNumberPrim[Int](e, true) {
  protected override def getNum(num: Number) = num.intValue
  protected override val GramName = "unsignedShort"
  protected override val GramDescription = "Unsigned Short"
  val min = 0L
  val max = (1L << 16) - 1L
}

case class ConvertTextUnsignedBytePrim(e: ElementBase) extends ConvertTextIntegerNumberPrim[Short](e, true) {
  protected override def getNum(num: Number) = num.shortValue
  protected override val GramName = "unsignedByte"
  protected override val GramDescription = "Unsigned Byte"
  val min = 0L
  val max = (1L << 8) - 1L
}

case class ConvertTextDoublePrim(e: ElementBase) extends ConvertTextFloatingPointNumberPrim[Double](e, true) {
  protected override def getNum(num: Number) = num.doubleValue
  protected override val GramName = "double"
  protected override val GramDescription = "Double"
  protected def isInvalidRange(n: java.lang.Number): Boolean = false
}

case class ConvertTextFloatPrim(e: ElementBase) extends ConvertTextFloatingPointNumberPrim[Float](e, true) {
  protected override def getNum(num: Number) = num.floatValue
  protected override val GramName = "float"
  protected override val GramDescription = "Float"
  protected def isInvalidRange(n: java.lang.Number): Boolean = {
    if (n == null) return false // tolerate null here. We catch that error elsewhere.
    val d = n.doubleValue()
    if (d.isNaN) false
    else (d < Float.MinValue) || d > Float.MaxValue
  }

}

//Is this different from ConvertTextNumberPrim?
//case class ConvertTextDoublePrim1(e: ElementBase) extends Terminal(e, true) {
//
//  def parser: Parser = new Parser(e) {
//    override def toString = "to(xs:double)"
//
//    def parse(start: PState): PState = {
//      val node = start.parentElement
//      val str = node.getText
//
//      val resultState =
//        // try 
//        {
//          //convert to NumberFormat to handle format punctuation such as , . $ & etc
//          //then get the value as a double and convert to string
//          val df = new DecimalFormat()
//          val pos = new ParsePosition(0)
//          val num = df.parse(str, pos)
//          node.setText(num.doubleValue.toString)
//
//          start
//        } // catch { case e: Exception => start.failed("Failed to convert to an xs:double") }
//
//      resultState
//    }
//  }
//
//  def unparser: Unparser = new Unparser(e) {
//    def unparse(start: UState): UState = {
//      Assert.notYetImplemented()
//    }
//  }
//}

//Is this different from ConvertTextNumberPrim?
//case class ConvertTextFloatPrim1(e: ElementBase) extends Terminal(e, true) {
//
//  def parser: Parser = new Parser(e) {
//    override def toString = "to(xs:float)"
//
//    def parse(start: PState): PState = {
//      val node = start.parentElement
//      val str = node.getText()
//
//      val resultState =
//        // Note: don't wrap in try-catch. The framework has everything surrounded
//        // by that already.
//        // try 
//        {
//          //convert to NumberFormat to handle format punctuation such as , . $ & etc
//          //then get the value as a float and convert to string
//          val df = new DecimalFormat()
//          val pos = new ParsePosition(0)
//          val num = df.parse(str, pos)
//          node.setText(num.floatValue.toString)
//          start
//        } // catch { case e: Exception => start.failed("Failed to convert to an xs:float") }
//
//      resultState
//    }
//  }
//
//  def unparser: Unparser = new Unparser(e) {
//    def unparse(start: UState): UState = {
//      Assert.notYetImplemented()
//    }
//  }
//}

abstract class Primitive(e: AnnotatedSchemaComponent, guard: Boolean = false)
  extends Terminal(e, guard) {
  override def toString = "Prim[" + name + "]"
  def parser: Parser = DummyParser(e)
  def unparser: Unparser = DummyUnparser(e)
}

abstract class ZonedTextNumberPrim(e: ElementBase, guard: Boolean) extends Terminal(e, guard) {
  def parser: Parser = new Parser(e) {
    def parse(start: PState): PState = {
      Assert.notYetImplemented()
    }
  }

  def unparser: Unparser = new Unparser(e) {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}
case class ZonedTextBytePrim(el: ElementBase) extends ZonedTextNumberPrim(el, false)
case class ZonedTextShortPrim(el: ElementBase) extends ZonedTextNumberPrim(el, false)
case class ZonedTextIntPrim(el: ElementBase) extends ZonedTextNumberPrim(el, false)
case class ZonedTextLongPrim(el: ElementBase) extends ZonedTextNumberPrim(el, false)

abstract class BinaryNumber[T](e: ElementBase, nBits: Long) extends Terminal(e, true) {
  lazy val primName = e.primType.name

  lazy val staticByteOrderString = e.byteOrder.constantAsString
  lazy val staticByteOrder = ByteOrder(staticByteOrderString, context)

  lazy val (staticJByteOrder, label) = staticByteOrder match {
    case ByteOrder.BigEndian => (java.nio.ByteOrder.BIG_ENDIAN, "BE")
    case ByteOrder.LittleEndian => (java.nio.ByteOrder.LITTLE_ENDIAN, "LE")
  }

  def getNum(bitPos: Long, inStream: InStream, byteOrder: java.nio.ByteOrder): T
  def getNum(t: Number): T
  override def toString = "binary(xs:" + primName + ", " + label + ")"
  val gram = this

  protected val GramName = "binary"
  protected val GramDescription = "Binary"
  protected def numFormat: NumberFormat
  protected def isInt: Boolean
  protected def isInvalidRange(n: T): Boolean = false

  def parser = new Parser(e) {
    override def toString = gram.toString

    def parse(start: PState): PState = {
      if (start.bitLimit != -1L && (start.bitLimit - start.bitPos < nBits)) start.failed("Not enough bits to create an xs:" + primName)
      else {
        val value = getNum(start.bitPos, start.inStream, staticJByteOrder)
        if (GramName == "hexBinary") {
          val bytes = value.asInstanceOf[Array[Byte]]
          var asString: StringBuilder = new StringBuilder()
          for (i <- 0 until bytes.length) {
            val byte = String.format("%02X", bytes(i).asInstanceOf[java.lang.Byte])
            asString.append(byte)
          }
          start.parentForAddContent.addContent(new org.jdom.Text(asString.toString()))
        } else
          start.parentForAddContent.addContent(new org.jdom.Text(value.toString))
        val postState = start.withPos(start.bitPos + nBits, -1)
        postState
      }
    }
  }

  def unparser = new Unparser(e) {
    override def toString = gram.toString

    def unparse(start: UState): UState = {
      setLoggingLevel(LogLevel.Debug)
      val str = start.currentElement.getText //gets data from element being unparsed

      Assert.invariant(str != null) // worst case it should be empty string. But not null.

      val postState = {
        if (str == "") return UE(start, "Convert to %s (for xs:%s): Cannot unparse number from empty string", GramDescription, GramName)
        else if (GramName == "hexBinary") {
          //regex to split string into array of two char elements ('bytes')
          val strArray = str.split("(?<=\\G..)")
          var asBytes = new Array[Byte](strArray.size)

          for (i <- 0 until strArray.size) {
            asBytes(i) = {
              val asInt = Integer.parseInt(strArray(i), 16)
              (asInt & 0xFF).byteValue()
            }
          }
          start.outStream.fillByteBuffer(asBytes, "hexBinary", staticJByteOrder) //write number back to ByteBuffer
          start

        } else {
          val df = numFormat
          val pos = new ParsePosition(0)
          val num = try {
            df.parse(str, pos)
          } catch {
            case e: Exception =>
              return UE(start, "Convert to %s (for xs:%s): Parse of '%s' threw exception %s",
                GramDescription, GramName, str, e)
          }

          // Verify that what was unparsed was what was passed exactly in byte count
          if (pos.getIndex != str.length) {
            return UE(start, "Convert to %s (for xs:%s): Unable to unparse '%s' (using up all characters).",
              GramDescription, GramName, str)
          }

          // convert to proper type
          val asNumber = getNum(num)

          // Verify no digits lost (the number was correctly transcribed)
          if (isInt && asNumber.asInstanceOf[Number] != num) { //then transcription error
            return UE(start, "Convert to %s (for xs:%s): Invalid data: '%s' unparsed into %s, which converted into %s.",
              GramDescription, GramName, str, num, asNumber)
          }
          if (isInvalidRange(asNumber)) {
            return UE(start, "Convert to %s (for xs:%s): Out of Range: '%s' converted to %s, is not in range for the type.",
              GramDescription, GramName, str, asNumber)
          }

          start.outStream.fillByteBuffer(asNumber, GramName, staticJByteOrder) //write number back to ByteBuffer
          start
        }
      }
      postState
    }
  }
}

//class Regular32bitIntPrim(context: Term, val byteOrder: java.nio.ByteOrder) 
//extends RegularNBitPrim[Int](context, 32, "int") {
//  def getNum(bitPos : Long, inStream : InStream) : Number =
//    inStream.getInt(bitPos, byteOrder)
//}
//
//class Regular32bitIntPrim1(context: Term, byteOrder: java.nio.ByteOrder) extends Parser(context) with Logging {
//  override def toString = "binary(xs:int, " + byteOrder + ")"
//
//  def parse(start: PState): PState = {
//    if (start.bitLimit != -1L && (start.bitLimit - start.bitPos < 32)) start.failed("Not enough bits to create an xs:int")
//    else {
//      val value = start.inStream.getInt(start.bitPos, byteOrder)
//      start.parentForAddContent.addContent(new org.jdom.Text(value.toString))
//      val postState = start.withPos(start.bitPos + 32, -1)
//      postState
//    }
//  }
//}
//
//class Regular32bitIntPrimUnparse(context: Term, byteOrder: java.nio.ByteOrder) extends Unparser(context) with Logging {
//  override def toString = "binary(xs:int, " + byteOrder + ")"
//
//  def unparse(start: UState): UState = {
//    Assert.notYetImplemented()
//  }
//}

// No point in specializing byte order. Java libs underneath just
// parameterize on it again.
//case class Regular32bitBigEndianIntPrim(e: ElementBase) extends Terminal(e, true) {
//  def parser = new Regular32bitIntPrim(e, java.nio.ByteOrder.BIG_ENDIAN)
//  def unparser = new Regular32bitIntPrimUnparse(e, java.nio.ByteOrder.BIG_ENDIAN)
//}
//case class Regular32bitLittleEndianIntPrim(e: ElementBase) extends Terminal(e, true) {
//  def parser = new Regular32bitIntPrim(e, java.nio.ByteOrder.LITTLE_ENDIAN)
//  def unparser = new Regular32bitIntPrimUnparse(e, java.nio.ByteOrder.LITTLE_ENDIAN)
//}

case class PackedIntPrim(e: ElementBase) extends Primitive(e, false)
case class BCDIntPrim(e: ElementBase) extends Primitive(e, false)

//case class DoublePrim(ctx: Term, byteOrder: java.nio.ByteOrder) extends Parser(ctx) {
//  override def toString = "binary(xs:double, " + byteOrder + ")"
//
//  def parse(start: PState): PState = {
//    if (start.bitLimit != -1L && (start.bitLimit - start.bitPos < 64)) start.failed("Not enough bits to create an xs:double")
//    else {
//      val value = start.inStream.getDouble(start.bitPos, byteOrder)
//      start.parentForAddContent.addContent(new org.jdom.Text(value.toString))
//      log(Debug("Found binary double " + value))
//      log(Debug("Ended at bit position " + (start.bitPos + 64)))
//      //val postState = start.withPos(start.bitPos + 64, -1)
//      val postState = start.withPos(start.bitPos + 64, start.charPos + 1)
//      postState
//    }
//  }
//}
//
//case class DoublePrimUnparse(ctx: Term, byteOrder: java.nio.ByteOrder) extends Unparser(ctx) with Logging {
//  override def toString = "binary(xs:double, " + byteOrder + ")"
//
//  def unparse(start: UState): UState = {
//    Assert.notYetImplemented()
//  }
//}
//
//case class BigEndianDoublePrim(e: ElementBase) extends Terminal(e, true) {
//  def parser = new DoublePrim(e, java.nio.ByteOrder.BIG_ENDIAN)
//  def unparser = new DoublePrimUnparse(e, java.nio.ByteOrder.BIG_ENDIAN)
//}
//
//case class LittleEndianDoublePrim(e: ElementBase) extends Terminal(e, true) {
//  def parser = new DoublePrim(e, java.nio.ByteOrder.LITTLE_ENDIAN)
//  def unparser = new DoublePrimUnparse(e, java.nio.ByteOrder.LITTLE_ENDIAN)
//}

case class FloatPrim(ctx: Term, byteOrder: java.nio.ByteOrder) extends Parser(ctx) {
  override def toString = "binary(xs:float,you " + byteOrder + ")"

  def parse(start: PState): PState = {
    if (start.bitLimit != -1L && (start.bitLimit - start.bitPos < 32)) start.failed("Not enough bits to create an xs:float")
    else {
      val value = start.inStream.getFloat(start.bitPos, byteOrder)
      start.parentForAddContent.addContent(new org.jdom.Text(value.toString))
      val postState = start.withPos(start.bitPos + 32, -1)
      postState
    }
  }
}

case class FloatPrimUnparse(ctx: Term, byteOrder: java.nio.ByteOrder) extends Unparser(ctx) {
  override def toString = "binary(xs:float, " + byteOrder + ")"

  def unparse(start: UState): UState = {
    Assert.notYetImplemented()
  }
}

case class BigEndianFloatPrim(e: ElementBase) extends Terminal(e, true) {
  def parser = new FloatPrim(e, java.nio.ByteOrder.BIG_ENDIAN)
  def unparser = new FloatPrimUnparse(e, java.nio.ByteOrder.BIG_ENDIAN)
}

case class LittleEndianFloatPrim(e: ElementBase) extends Terminal(e, true) {
  def parser = new FloatPrim(e, java.nio.ByteOrder.LITTLE_ENDIAN)
  def unparser = new FloatPrimUnparse(e, java.nio.ByteOrder.LITTLE_ENDIAN)
}

abstract class StaticDelimiter(delim: String, e: Term, guard: Boolean = true)
  extends StaticText(delim, e, guard)

abstract class StaticText(delim: String, e: Term, guard: Boolean = true)
  extends Terminal(e, guard)
  with WithParseErrorThrowing {
  lazy val es = e.escapeScheme
  lazy val esObj = EscapeScheme.getEscapeScheme(es, e)

  val term = e.asInstanceOf[Term]
  lazy val separators = delim.split("\\s").toList
  lazy val terminators = term.allTerminatingMarkup.map(x => x.constantAsString)
  lazy val terminatorsFiltered = terminators.filterNot(x => separators.contains(x))

  val separatorsCooked: Queue[String] = new Queue
  val terminatorsCooked: Queue[String] = new Queue

  separators.foreach(x => separatorsCooked.enqueue(EntityReplacer.replaceAll(x)))
  terminatorsFiltered.foreach(x => terminatorsCooked.enqueue(EntityReplacer.replaceAll(x)))

  def parser: Parser = new Parser(e) {

    Assert.notYetImplemented(e.ignoreCase == YesNo.Yes)

    Assert.invariant(delim != "") // shouldn't be here at all in this case.
    override def toString = "StaticText('" + delim + "' with terminating markup: " + term.prettyTerminatingMarkup + ")"
    val decoder = e.knownEncodingDecoder

    def parse(start: PState): PState = withParseErrorThrowing(start) {
      withLoggingLevel(LogLevel.Info) {
        val eName = e.toString()

        log(Debug(eName + " - Parsing delimiter at byte position: " + (start.bitPos >> 3)))
        log(Debug(eName + " - Parsing delimiter at bit position: " + start.bitPos))

        val in: InStreamFromByteChannel = start.inStream.asInstanceOf[InStreamFromByteChannel]

        val bytePos = (start.bitPos >> 3).toInt

        val byteReader = in.byteReader.atPos(bytePos)
        val reader = byteReader.charReader(decoder.charset().name())

        val d = new delimsearch.DelimParser()

        var result: delimsearch.DelimParseResult = new delimsearch.DelimParseResult

        result = d.parseInput(separatorsCooked.toSet, terminatorsCooked.toSet, reader, decoder.charset())

        if (!result.isSuccess) {
          return start.failed(this.toString() + " - " + eName + ": Delimiter not found!")
        } else if (result.delimiterType == delimsearch.DelimiterType.Delimiter || result.delimiterType == delimsearch.DelimiterType.Terminator) {
          return start.failed(this.toString() + " - " + eName + ": Delimiter not found!")
        } else {
          val numBytes = result.delimiter.getBytes(decoder.charset()).length
          val endCharPos = start.charPos + result.field.length()
          val endBitPosDelim = (8 * numBytes) + start.bitPos

          log(Debug(eName + " - Found " + result.delimiter))
          log(Debug(eName + " - Ended at byte position " + (endBitPosDelim >> 3)))
          log(Debug(eName + " - Ended at bit position " + endBitPosDelim))

          return start.withPos(endBitPosDelim, endCharPos)
        }
        start
      }
    }
  }

  def unparser: Unparser = new Unparser(e) {
    val t = e.asInstanceOf[Term]
    override def toString = "StaticText('" + delim + "' with terminating markup: " + t.prettyTerminatingMarkup + ")"
    // setLoggingLevel(LogLevel.Debug)
    Assert.notYetImplemented(e.ignoreCase == YesNo.Yes)
    Assert.invariant(delim != "") //shouldn't be here at all in this case

    def unparse(start: UState): UState = {
      val encoder = e.knownEncodingEncoder
      start.outStream.setEncoder(encoder)
      start.outStream.fillCharBuffer(unparserDelim)
      log(Debug("Unparsed: " + start.outStream.getData))
      start
    }
  }

  def unparserDelim: String
}

class DynamicDelimiter(delimExpr: CompiledExpression, e: Term, guard: Boolean = true) extends Primitive(e, guard)

//case class StaticInitiator(e: Term) extends StaticDelimiter(e.initiator.constantAsString, e)
case class StaticInitiator(e: Term) extends StaticDelimiter(e.initiator.constantAsString, e) {
  Assert.invariant(e.hasInitiator)
  lazy val unparserDelim = e.initiator.constantAsString.split("""\s""").head
}
//case class StaticTerminator(e : Term) extends StaticDelimiter(e.terminator.constantAsString, e)
case class StaticTerminator(e: Term) extends StaticDelimiter(e.terminator.constantAsString, e) {
  Assert.invariant(e.hasTerminator)
  lazy val unparserDelim = e.terminator.constantAsString.split("""\s""").head
}
case class DynamicInitiator(e: Term) extends DynamicDelimiter(e.initiator, e)
case class DynamicTerminator(e: Term) extends DynamicDelimiter(e.terminator, e)

case class StaticSeparator(s: Sequence, t: Term) extends StaticDelimiter(s.separator.constantAsString, t) {
  Assert.invariant(s.hasSeparator)
  lazy val unparserDelim = s.separator.constantAsString.split("""\s""").head
}
case class DynamicSeparator(s: Sequence, t: Term) extends DynamicDelimiter(s.separator, t)

case class StartChildren(ct: ComplexTypeBase, guard: Boolean = true) extends Terminal(ct.element, guard) {

  def parser: Parser = new Parser(ct.element) {
    override def toString = "StartChildren"

    def parse(start: PState): PState = {
      val postState = start.withChildIndexStack(1L :: start.childIndexStack)
      postState
    }
  }

  def unparser: Unparser = new Unparser(ct.element) {
    override def toString = "StartChildren"

    def unparse(start: UState): UState = {
      val postState = start.withChildIndexStack(1L :: start.childIndexStack)
      postState
    }
  }
}

case class StartSequence(sq: Sequence, guard: Boolean = true) extends Terminal(sq, guard) {

  def parser: Parser = new Parser(sq) {
    override def toString = "StartSequence"

    def parse(start: PState): PState = {
      val postState = start.withGroupIndexStack(1L :: start.groupIndexStack)
      postState
    }
  }

  def unparser: Unparser = new Unparser(sq) {
    override def toString = "StartSequence"

    def unparse(start: UState): UState = {
      val postState = start.withGroupIndexStack(1L :: start.groupIndexStack)
      postState
    }
  }
}

case class Nada(sc: Term) extends Terminal(sc, true) {
  override def isEmpty = false
  // cannot optimize this out! It is used as an alternative to things
  // with the intention of "find this and this, or find nothing"

  def parser: Parser = new Parser(sc) {
    override def toString = "Nada"

    def parse(start: PState): PState = start
  }

  def unparser: Unparser = new Unparser(sc) {
    override def toString = "Nada"

    def unparse(start: UState): UState = start
  }
}

case class GroupPosGreaterThan(groupPos: Long, term: Term, guard: Boolean = true) extends Terminal(term, guard) {

  def parser: Parser = new Parser(term) {
    override def toString = "GroupPosGreaterThan(" + groupPos + ")"

    def parse(start: PState): PState = {
      val res = if (start.groupPos > groupPos) {
        start.withDiscriminator(true)
      } else {
        start.failed("Group position not greater than (" + groupPos + ")")
      }
      res
    }
  }

  def unparser: Unparser = new Unparser(term) {
    override def toString = "GroupPosGreaterThan(" + groupPos + ")"

    def unparse(start: UState): UState = {
      val res = if (start.groupPos > groupPos) {
        start.withDiscriminator(true)
      } else {
        start.failed("Group position not greater than (" + groupPos + ")")
      }
      res
    }
  }
}

case class ChildPosGreaterThan(childPos: Long, term: Term, guard: Boolean = true) extends Terminal(term, guard) {

  def parser: Parser = new Parser(term) {
    override def toString = "ChildPosGreaterThan(" + childPos + ")"

    def parse(start: PState): PState = {
      val res = if (start.childPos > childPos) {
        start.withDiscriminator(true)
      } else {
        start.failed("Child position not greater than (" + childPos + ")")
      }
      res
    }
  }

  def unparser: Unparser = new Unparser(term) {
    override def toString = "ChildPosGreaterThan(" + childPos + ")"

    def unparse(start: UState): UState = {
      val res = if (start.childPos > childPos) {
        start.withDiscriminator(true)
      } else {
        start.failed("Child position not greater than (" + childPos + ")")
      }
      res
    }
  }
}

case class ArrayPosGreaterThan(arrayPos: Long, term: Term, guard: Boolean = true) extends Terminal(term, guard) {

  def parser: Parser = new Parser(term) {
    override def toString = "ArrayPosGreaterThan(" + arrayPos + ")"

    def parse(start: PState): PState = {
      val res = try {
        if (start.arrayPos > arrayPos) {
          start.withDiscriminator(true)
        } else {
          start.failed("Array position not greater than (" + arrayPos + ")")
        }
      } catch { case e => start.failed("No array position") }
      res
    }
  }

  def unparser: Unparser = new Unparser(term) {
    override def toString = "ArrayPosGreaterThan(" + arrayPos + ")"

    def unparse(start: UState): UState = {
      val res = try {
        if (start.arrayPos > arrayPos) {
          start.withDiscriminator(true)
        } else {
          start.failed("Array position not greater than (" + arrayPos + ")")
        }
      } catch { case e => start.failed("No array position") }
      res
    }
  }
}

case class EndChildren(ct: ComplexTypeBase, guard: Boolean = true) extends Terminal(ct.element, guard) {

  def parser: Parser = new Parser(ct.element) {
    override def toString = "EndChildren"

    def parse(start: PState): PState = {
      val postState = start.withChildIndexStack(start.childIndexStack.tail)
      postState
    }
  }

  def unparser: Unparser = new Unparser(ct.element) {
    override def toString = "EndChildren"

    def unparse(start: UState): UState = {
      val postState = start.withChildIndexStack(start.childIndexStack.tail)
      postState
    }
  }
}

case class EndSequence(sq: Sequence, guard: Boolean = true) extends Terminal(sq, guard) {

  def parser: Parser = new Parser(sq) {
    override def toString = "EndSequence"

    def parse(start: PState): PState = {
      val postState = start.withGroupIndexStack(start.groupIndexStack.tail)
      postState
    }
  }

  def unparser: Unparser = new Unparser(sq) {
    override def toString = "EndSequence"

    def unparse(start: UState): UState = {
      val postState = start.withGroupIndexStack(start.groupIndexStack.tail)
      postState
    }
  }
}

case class StartArray(e: ElementBase, guard: Boolean = true) extends Terminal(e, guard) {

  def parser: Parser = new Parser(e) {
    override def toString = "StartArray"

    def parse(start: PState): PState = {
      val postState1 = start.withArrayIndexStack(1L :: start.arrayIndexStack)
      val postState2 = postState1.withOccursCountStack(Compiler.occursCountMax :: postState1.occursCountStack)
      postState2
    }
  }

  def unparser: Unparser = new Unparser(e) {
    override def toString = "StartArray"

    def unparse(start: UState): UState = {
      val postState = start.withArrayIndexStack(1L :: start.arrayIndexStack)
      postState
    }
  }
}

case class EndArray(e: ElementBase, guard: Boolean = true) extends Terminal(e, guard) {

  def parser: Parser = new Parser(e) {
    override def toString = "EndArray"

    def parse(start: PState): PState = {
      val postState1 = start.withArrayIndexStack(start.arrayIndexStack.tail)
      val postState2 = postState1.withOccursCountStack(postState1.occursCountStack.tail)
      postState2
    }
  }

  def unparser: Unparser = new Unparser(e) {
    override def toString = "EndArray"

    def unparse(start: UState): UState = {
      val postState = start.withArrayIndexStack(start.arrayIndexStack.tail)
      postState
    }
  }
}

case class NoValue(e: GlobalElementDecl, guard: Boolean = true) extends Primitive(e, guard)

case class SaveInputStream(e: ElementBase, guard: Boolean = true) extends Primitive(e, guard)

case class SetEmptyInputStream(e: ElementBase, guard: Boolean = true) extends Primitive(e, guard)

case class RestoreInputStream(e: ElementBase, guard: Boolean = true) extends Primitive(e, guard)

//case class Value(e: SchemaComponent, guard: Boolean = true) extends Primitive(e, guard) 

case class NotStopValue(e: ElementBase with LocalElementMixin) extends Primitive(e, e.hasStopValue)

case class StopValue(e: ElementBase with LocalElementMixin) extends Primitive(e, e.hasStopValue)

case class TheDefaultValue(e: ElementBase) extends Primitive(e, e.isDefaultable)

case class LiteralNilValue(e: ElementBase)
  extends StaticText(e.nilValue, e, e.isNillable) {
  lazy val unparserDelim = Assert.notYetImplemented()
  //  lazy val esObj = EscapeScheme.getEscapeScheme(es, e)
  val stParser = super.parser

  override def parser = new Parser(e) {
    override def toString = "LiteralNilValue(" + e.nilValue + ")"
    val decoder = e.knownEncodingDecoder

    def parse(start: PState): PState = {
      withLoggingLevel(LogLevel.Info) {
        val eName = e.toString()

        // We must feed variable context out of one evaluation and into the next.
        // So that the resulting variable map has the updated status of all evaluated variables.
        var vars = start.variableMap
        val delimsRaw = e.allTerminatingMarkup.map {
          x =>
            {
              val R(res, newVMap) = x.evaluate(start.parent, vars)
              vars = newVMap
              res
            }
        }
        val delimsCooked1 = delimsRaw.map(raw => { new daffodil.dsom.ListOfStringValueAsLiteral(raw.toString, e).cooked })
        val delimsCooked = delimsCooked1.flatten
        val nilValuesCooked1 = delimsRaw.map(raw => { new daffodil.dsom.ListOfStringValueAsLiteral(e.nilValue, e).cooked })
        val nilValuesCooked = nilValuesCooked1.flatten
        val postEvalState = start.withVariables(vars)

        log(Debug(eName + " - Looking for: " + delimsCooked + " Count: " + delimsCooked.length))
        val in: InStreamFromByteChannel = start.inStream.asInstanceOf[InStreamFromByteChannel]

        val bytePos = (postEvalState.bitPos >> 3).toInt
        log(Debug(eName + " - Starting at bit pos: " + postEvalState.bitPos))
        log(Debug(eName + " - Starting at byte pos: " + bytePos))

        val byteReader = in.byteReader.atPos(bytePos)
        val reader = byteReader.charReader(decoder.charset().name())

        // 1. Parse up until terminating Markup
        // 2. Compare resultant field to nilValue(s)
        //		Same, success
        //		Diff, fail
        val d = new delimsearch.DelimParser()
        var result: delimsearch.DelimParseResult = new delimsearch.DelimParseResult

        if (esObj.escapeSchemeKind == stringsearch.constructs.EscapeSchemeKind.Block) {
          result = d.parseInputEscapeBlock(Set.empty[String], delimsCooked.toSet, reader,
            esObj.escapeBlockStart, esObj.escapeBlockEnd, esObj.escapeEscapeCharacter, decoder.charset())
        } else if (esObj.escapeSchemeKind == stringsearch.constructs.EscapeSchemeKind.Character) {
          result = d.parseInputEscapeCharacter(Set.empty[String], delimsCooked.toSet, reader,
            esObj.escapeCharacter, esObj.escapeEscapeCharacter, decoder.charset())
        } else {
          result = d.parseInput(Set.empty[String], delimsCooked.toSet, reader, decoder.charset())
        }

        if (!result.isSuccess) {
          return postEvalState.failed(this.toString() + " - " + eName + " - Parse failed.")
        } else {
          // We have a field, is it empty?
          val field = result.field
          val isFieldEmpty = field.length() == 0
          val isEmptyAllowed = e.nilValue.contains("%ES;")
          if (isFieldEmpty && isEmptyAllowed) {
            // Valid!
            val xsiNS = start.parentElement.getNamespace()
            start.parentElement.addContent(new org.jdom.Text(""))
            start.parentElement.setAttribute("nil", "true")
            return postEvalState // Empty, no need to advance
          } else if (isFieldEmpty && !isEmptyAllowed) {
            // Fail!
            return PE(postEvalState, eName + " - Empty field found but not allowed!")
          } else if (d.isFieldDfdlLiteral(field,nilValuesCooked.toSet)) {
            // Contains a nilValue, Success!
            val xsiNS = start.parentElement.getNamespace()
            start.parentElement.addContent(new org.jdom.Text(""))
            start.parentElement.setAttribute("nil", "true")
            
            val numBytes = result.field.getBytes(decoder.charset()).length
            val endCharPos = start.charPos + result.field.length()
            val endBitPos = (8 * numBytes) + start.bitPos

            log(Debug(eName + " - Found " + result.field))
            log(Debug(eName + " - Ended at byte position " + (endBitPos >> 3)))
            log(Debug(eName + " - Ended at bit position " + endBitPos))

            return postEvalState.withPos(endBitPos, endCharPos) // Need to advance past found nilValue
          } else {
            // Fail!
            return PE(postEvalState, eName + " - Does not contain a nil literal!")
          }
        }
      }
    }

  }

  override def unparser: Unparser = new Unparser(e) {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

case class LogicalNilValue(e: ElementBase) extends Primitive(e, e.isNillable)

// As soon as you turn these on (by removing the false and putting the real guard), then schemas all need to have
// these properties in them, which is inconvenient until we have multi-file schema support and format references.
case class LeadingSkipRegion(e: Term) extends Primitive(e, false) // e.leadingSkip > 0)

case class AlignmentFill(e: Term) extends Primitive(e, false) // e.alignment != AlignmentType.Implicit)

case class TrailingSkipRegion(e: Term) extends Primitive(e, false) // e.trailingSkip > 0)

case class PrefixLength(e: ElementBase) extends Primitive(e, e.lengthKind == LengthKind.Prefixed)

case class UnicodeByteOrderMark(e: GlobalElementDecl) extends Primitive(e, false)

case class FinalUnusedRegion(e: ElementBase) extends Primitive(e, false)

abstract class NewVariableInstanceBase(decl: AnnotatedSchemaComponent, stmt: DFDLNewVariableInstance)
  extends Terminal(decl, true) {
  val (uri, localName) = XMLUtils.QName(decl.xml, stmt.ref, decl.schemaDocument)
  val expName = XMLUtils.expandedQName(uri, localName)
}

case class NewVariableInstanceStart(decl: AnnotatedSchemaComponent, stmt: DFDLNewVariableInstance)
  extends NewVariableInstanceBase(decl, stmt) {

  def parser: Parser = new Parser(decl) {
    def parse(pstate: PState) = {
      Assert.notYetImplemented()
    }
  }

  def unparser: Unparser = Assert.notYetImplemented()

}

case class NewVariableInstanceEnd(decl: AnnotatedSchemaComponent, stmt: DFDLNewVariableInstance)
  extends NewVariableInstanceBase(decl, stmt) {

  def parser: Parser = new Parser(decl) {
    def parse(pstate: PState) = Assert.notYetImplemented()
  }

  def unparser: Unparser = Assert.notYetImplemented()
}

case class AssertPatternPrim(decl : AnnotatedSchemaComponent, stmt : DFDLAssert) extends Terminal(decl, true) {

  def parser : Parser = Assert.notYetImplemented()
  def unparser : Unparser = Assert.notYetImplemented()

}

abstract class AssertBase(
  decl : AnnotatedSchemaComponent,
  exprTextArg : String,
  msg : String,
  discrim : Boolean, // are we a discriminator or not.
  assertKindName : String
  )
  extends Terminal(decl, true) {
  
  def unparser = DummyUnparser

  def parser : Parser = new ExpressionEvaluationParser(decl) {
    val baseName = assertKindName

    lazy val exprText = exprTextArg
    lazy val expandedTypeName = XMLUtils.XSD_BOOLEAN
    def parse(start : PState) : PState =
      withLoggingLevel(LogLevel.Info) {
        withParseErrorThrowing(start) {
          log(Debug("This is %s", toString))
          val R(res, newVMap) = eval(start)
          val testResult = res.asInstanceOf[Boolean]
          val postState = start.withVariables(newVMap)
          if (testResult) {
            postState.withDiscriminator(discrim)
          } else {
            // The assertion failed. Prepare a failure message etc. in case backtracking ultimately fails from here.
            val diag = new AssertionFailed(decl, postState, msg)
            postState.failed(diag)
          }
        }
      }
  }
}

abstract class AssertBooleanPrimBase(
  decl : AnnotatedSchemaComponent,
  stmt : DFDLAssertionBase,
  discrim : Boolean, // are we a discriminator or not.
  assertKindName : String
  ) extends AssertBase(decl, stmt.testTxt, stmt.message, discrim, assertKindName)

case class AssertBooleanPrim(
  decl : AnnotatedSchemaComponent,
  stmt : DFDLAssertionBase)
  extends AssertBooleanPrimBase(decl, stmt, false, "assert") {
}

case class DiscriminatorBooleanPrim(
  decl : AnnotatedSchemaComponent,
  stmt : DFDLAssertionBase)
  extends AssertBooleanPrimBase(decl, stmt, true, "discriminator")

case class InitiatedContent(
  decl : AnnotatedSchemaComponent)
  extends AssertBase(decl, 
      "{ xs:boolean('true') }", // always true. We're just an assertion
      "initiatedContent. This message should not be used.", 
      true, 
      "initiatedContent")

case class SetVariable(decl : AnnotatedSchemaComponent, stmt : DFDLSetVariable) extends Terminal(decl, true) {
  def parser : Parser = new SetVariableParser(decl, stmt)
  def unparser = DummyUnparser
}

case class InputValueCalc(e: ElementBase with ElementDeclMixin) extends Terminal(e, true) {

  def parser : Parser = new IVCParser(e)
  def unparser = DummyUnparser
}

abstract class ExpressionEvaluationParser(e: AnnotatedSchemaComponent)
  extends Parser(e) with WithParseErrorThrowing {
  override def toString = baseName + "(" + exprText + ")"
  def baseName: String
  def exprText: String
  def expandedTypeName: String
  lazy val expressionTypeSymbol = {
    // println(expandedTypeName)
    e.expressionCompiler.convertTypeString(expandedTypeName)
  }

  lazy val expr = e.expressionCompiler.compile(expressionTypeSymbol, exprText)

  // for unit testing
  def testExpressionEvaluation(elem: org.jdom.Element, vmap: VariableMap) = {
    val R(res, newVMap) = expr.evaluate(elem, vmap)
    R(res, newVMap)
  }

  def eval(start: PState) = {
    val currentElement = start.parentElement
    val R(res, newVMap) =
      expr.evaluate(currentElement, start.variableMap)
    // val result = res.toString // Everything in JDOM is a string!
    R(res, newVMap)
  }
}

class IVCParser(e: ElementBase with ElementDeclMixin)
  extends ExpressionEvaluationParser(e) {
  Assert.invariant(e.isSimpleType)
  val baseName = "InputValueCalc"

  lazy val Some(exprText) = e.inputValueCalcOption

  lazy val pt = e.primType
  lazy val ptn = pt.name
  lazy val expandedTypeName = XMLUtils.expandedQName(XMLUtils.XSD_NAMESPACE, ptn)

  def parse(start: PState): PState =
    withLoggingLevel(LogLevel.Info) {
      withParseErrorThrowing(start) {
        log(Debug("This is %s", toString))
        val currentElement = start.parentElement
        val R(res, newVMap) = eval(start)
        currentElement.addContent(new org.jdom.Text(res.toString))
        val postState = start.withVariables(newVMap) // inputValueCalc consumes nothing. Just creates a value.
        postState
      }
    }
}

class SetVariableParser(decl: AnnotatedSchemaComponent, stmt: DFDLSetVariable)
  extends ExpressionEvaluationParser(decl) {
  val baseName = "SetVariable[" + localName + "]"
  lazy val exprText = stmt.value
  val (uri, localName) = XMLUtils.QName(decl.xml, stmt.ref, decl.schemaDocument)
  val defv = decl.schema.schemaSet.getDefineVariable(uri, localName).getOrElse(
    stmt.schemaDefinitionError("Unknown variable: %s", stmt.ref))

  lazy val expandedTypeName = defv.extType

  def parse(start: PState): PState =
    withLoggingLevel(LogLevel.Info) {
      withParseErrorThrowing(start) {
        log(Debug("This is %s", toString))
        val R(res, newVMap) = eval(start)
        val newVMap2 = newVMap.setVariable(defv.extName, res, decl)
        val postState = start.withVariables(newVMap2)
        postState
      }
    }
}

case class StringExplicitLengthInBytes(e: ElementBase)
  extends Terminal(e, true)
  with WithParseErrorThrowing {
  val expr = e.length
  val exprText = expr.prettyExpr
  val decoder = e.knownEncodingDecoder
  // val maxBytes = daffodil.compiler.Compiler.maxFieldContentLengthInBytes
  var cbuf: CharBuffer = CharBuffer.allocate(0) // TODO: Performance: get a char buffer from a pool.
  var cbufSize = 0

  def parser: Parser = new Parser(e) {
    override def toString = "StringExplicitLengthInBytesParser(" + exprText + ")"

    def parse(pstate: PState): PState = withParseErrorThrowing(pstate) {
      log(Debug("Parsing starting at bit position: %s", pstate.bitPos))
      val R(nBytesAsAny, newVMap) = expr.evaluate(pstate.parent, pstate.variableMap)
      val nBytes = nBytesAsAny.asInstanceOf[Long]
      val start = pstate.withVariables(newVMap)
      log(Debug("Explicit length %s", nBytes))

      val in: InStreamFromByteChannel = start.inStream.asInstanceOf[InStreamFromByteChannel]

      val bytePos = (start.bitPos >> 3).toInt

      try {
        val bytes = in.getBytes(start.bitPos, nBytes.toInt)
        val cb = decoder.decode(ByteBuffer.wrap(bytes))
        val result = cb.toString
        val endBitPos = start.bitPos + (nBytes.toInt * 8)
        log(Debug("Parsed: " + result))
        log(Debug("Ended at bit position " + endBitPos))
        val endCharPos = start.charPos + result.length
        val currentElement = start.parentForAddContent
        // Assert.invariant(currentElement.getName != "_document_")
        // Note: this side effect is backtracked, because at points of uncertainty, pre-copies of a node are made
        // and when backtracking occurs they are used to replace the nodes modified by sub-parsers.
        currentElement.addContent(new org.jdom.Text(result))
        val postState = start.withPos(endBitPos, endCharPos)
        return postState
      } catch {
        case e: java.nio.BufferUnderflowException => { return PE(start, "StringExplicitLengthInBytesParser - Insufficient Bits in field; required " + nBytes * 8) }
        case e: IndexOutOfBoundsException => { return PE(start, "StringExplicitLengthInBytesParser - IndexOutOfBounds: " + e.getMessage()) }
        case e: Exception => { return start.failed("StringExplicitLengthInBytesParser - Exception: " + e.getStackTraceString) }
      }

      //      log(Debug("Parsing starting at bit position: %s", pstate.bitPos))
      //      val R(nBytesAsAny, newVMap) = expr.evaluate(pstate.parent, pstate.variableMap)
      //      val nBytes = nBytesAsAny.asInstanceOf[Long]
      //      val start = pstate.withVariables(newVMap)
      //      log(Debug("Explicit length %s", nBytes))
      //
      //      // Allocate larger cbuf on demand.
      //      if (cbufSize < nBytes) { // worst case here 1 byte = 1 character
      //        if (nBytes > Compiler.maxFieldContentLengthInBytes) {
      //          // TODO: how can we go after bigger than max int bytes? We have 64-bit computers
      //          // after all....
      //          return PE(start, "Calculated length %s exceeds implementation maximum of %s.", nBytes, Compiler.maxFieldContentLengthInBytes)
      //        }
      //        val n : Int = nBytes.toInt
      //        cbuf = CharBuffer.allocate(n)
      //        cbufSize = n
      //      }
      //
      //      val in = start.inStream
      //
      //      val (endBitPos, _) = in.fillCharBufferMixedData(cbuf, start.bitPos, decoder, nBytes)
      //      if (endBitPos < start.bitPos + nBytes * 8) {
      //        // Do Something Bad
      //        return PE(start, "Insufficent Bits in field; required " + nBytes * 8 + " received " + (endBitPos - start.bitPos))
      //      }
      //      val result = cbuf.toString
      //      log(Debug("Parsed: " + result))
      //      log(Debug("Ended at bit position " + endBitPos))
      //      val endCharPos = start.charPos + result.length
      //      val currentElement = start.parentForAddContent
      //      // Note: this side effect is backtracked, because at points of uncertainty, pre-copies of a node are made
      //      // and when backtracking occurs they are used to replace the nodes modified by sub-parsers.
      //      currentElement.addContent(new org.jdom.Text(result))
      //      val postState = start.withPos(endBitPos, endCharPos)
      //      postState
      pstate
    }
  }

  def unparser: Unparser = new Unparser(e) {
    override def toString = "StringExplicitLengthInBytesUnparser(" + exprText + ")"
    //    val encoder = e.knownEncodingEncoder

    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
      //      // setLoggingLevel(LogLevel.Debug)
      //
      //      val data = start.currentElement.getText
      //
      //      //      start.outStream.setEncoder(encoder)
      //      start.outStream.fillCharBuffer(data)
      //
      //      log(Debug("Unparsed: " + start.outStream.getData))
      //      start
    }
  }
}



