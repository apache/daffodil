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
import daffodil.exceptions.UnsuppressableException
import stringsearch.constructs.{ EscapeScheme, SearchResult }
import stringsearch.delimiter.Delimiter
import com.ibm.icu.text.{ NumberFormat, DecimalFormat }
import daffodil.grammar.Terminal
import scala.util.parsing.input.{ Reader }
import delimsearch.DFDLCharReader
import java.sql.Timestamp
import daffodil.grammar.Gram

abstract class PrimParser(gram : Gram, context : SchemaComponent) extends Parser(context) {

  def toBriefXML(depthLimit : Int = -1) : String = {
    "<" + gram.name + "/>"
  }

  override def toString = toBriefXML()

}

case class ElementBegin(e : ElementBase) extends Terminal(e, e.isComplexType != true || e.lengthKind != LengthKind.Pattern) {

  def parser : Parser = new PrimParser(this, e) {

    override def toBriefXML(depthLimit : Int = -1) : String = {
      "<ElementBegin name='" + e.name + "'/>"
    }

    /**
     * ElementBegin just adds the element we are constructing to the infoset and changes
     * the state to be referring to this new element as what we're parsing data into.
     */
    def parse(start : PState) : PState = {
      val currentElement = new org.jdom.Element(e.name, e.targetNamespacePrefix, e.targetNamespace)
      log(Debug("currentElement = %s", currentElement))
      val priorElement = start.parentForAddContent
      priorElement.addContent(currentElement)
      log(Debug("priorElement = %s", priorElement))
      val postState = start.withParent(currentElement)
      postState
    }
  }

  def unparser : Unparser = new Unparser(e) {
    override def toString = "<" + e.name + ">"

    /**
     * Changes the state to refer to the next element in the infoset as the element to unparse.
     */
    def unparse(start : UState) : UState = {
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
          case u : UnsuppressableException => throw u
          case e : Exception => start.currentElement //if content is text
        }
      }

      start.withCurrent(nextElement).moveOverByOneElement
    }
  }
}

case class ComplexElementBeginPattern(e : ElementBase)
  extends Terminal(e, e.isComplexType == true && e.lengthKind == LengthKind.Pattern)
  with WithParseErrorThrowing {
  Assert.invariant(e.isComplexType)

  def parser : Parser = new PrimParser(this, e) {
    override def toString = "<" + e.name + " dfdl:lengthKind='pattern'>"
    val decoder = e.knownEncodingDecoder
    var cbuf = CharBuffer.allocate(1024) // TODO: Performance: get a char buffer from a pool.
    val pattern = e.lengthPattern

    /**
     * ElementBegin just adds the element we are constructing to the infoset and changes
     * the state to be referring to this new element as what we're parsing data into.
     */
    def parse(start : PState) : PState = withParseErrorThrowing(start) {
      withLoggingLevel(LogLevel.Info) {
        val eName = e.toString()

        log(Debug("ComplexElementBeginPattern - %s - Parsing pattern at byte position: %s", eName, (start.bitPos >> 3)))
        log(Debug("ComplexElementBeginPattern - %s - Parsing pattern at bit position: %s", eName, start.bitPos))

        val in : InStreamFromByteChannel = start.inStream.asInstanceOf[InStreamFromByteChannel]

        val bytePos = (start.bitPos >> 3).toInt

        val byteReader = in.byteReader.atPos(bytePos)
        val reader = byteReader.charReader(decoder.charset().name())

        val d = new delimsearch.DelimParser()

        var result : delimsearch.DelimParseResult = new delimsearch.DelimParseResult

        result = d.parseInputPatterned(pattern, reader, decoder.charset())

        val postState1 = result.isSuccess match {
          case true => {
            val endBitPos = start.bitPos + (result.numBytes * 8)
            log(Debug("Parsed: %s", result.field))
            log(Debug("Ended at bit position %s", endBitPos))
            val limitedInStream = in.withLimit(start.bitPos, endBitPos)
            // Since we've created a new sub-stream with just the limited part of data in it,
            // don't forget to have the position in it start at zero.
            start withEndBitLimit (endBitPos) withInStream (new InStreamFromByteChannel(e, limitedInStream)) withPos (0, 0)
          }
          case false => { PE(start, "%s: No match found!", this.toString()) }
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

  def unparser : Unparser = new Unparser(e) {
    def unparse(start : UState) : UState = {
      Assert.notYetImplemented()
    }
  }
}

abstract class ElementEndBase(e : ElementBase) extends Terminal(e, e.isComplexType != true || e.lengthKind != LengthKind.Pattern) {
  def toPrettyString = "</" + e.name + prettyStringModifier + ">"
  def prettyStringModifier : String

  def move(pstate : PState) : PState // implement for different kinds of "moving over to next thing"

  def parser : Parser = new PrimParser(this, e) {

    override def toBriefXML(depthLimit : Int = -1) : String = {
      "<ElementEnd name='" + e.name + "'/>"
    }

    override def toString = toPrettyString

    /**
     * ElementEnd just moves back to the parent element of the current one.
     */
    def parse(start : PState) : PState = {
      val currentElement = start.parent
      // Assert.invariant(currentElement.getName() != "_document_" )
      val priorElement = currentElement.getParent
      log(Debug("priorElement = %s", priorElement))
      val postState = move(start.withParent(priorElement))
      postState
    }
  }

  def unparser : Unparser = new Unparser(e) {
    override def toString = "</" + e.name + ">"

    /**
     * Changes state to refer to parent element of the current one.
     */
    def unparse(start : UState) : UState = {
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

case class ElementEnd(e : ElementBase) extends ElementEndBase(e) {
  def move(pstate : PState) = pstate.moveOverByOneElement
  def prettyStringModifier = ""
}

case class ElementEndNoRep(e : ElementBase) extends ElementEndBase(e) {
  def move(pstate : PState) = pstate.moveOverOneElementChildOnly
  def prettyStringModifier = "(NoRep)"
}

case class ComplexElementEndPattern(e : ElementBase) extends Terminal(e, e.isComplexType == true && e.lengthKind == LengthKind.Pattern) {
  // TODO: Should this be more generic; is there a way to detect state from the current element to tell us if it's time
  //       to pop the input stack?

  def parser : Parser = new PrimParser(this, e) {
    override def toString = "</" + e.name + " dfdl:lengthKind='pattern'>"

    /**
     * ElementEnd just moves back to the parent element of the current one.
     */
    def parse(start : PState) : PState = {
      val currentElement = start.parent
      log(Debug("currentElement = %s", currentElement))
      // Assert.invariant(currentElement.getName() != "_document_" )
      //val priorElement = currentElement.getParent.asInstanceOf[org.jdom.Element]
      var priorElement = currentElement.getParent
      log(Debug("priorElement = %s", priorElement))
      val postState = start.withParent(priorElement).moveOverByOneElement.withLastInStream()
      postState
    }
  }

  def unparser : Unparser = new Unparser(e) {
    def unparse(start : UState) : UState = {
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

case class StringFixedLengthInBytes(e : ElementBase, nBytes : Long)
  extends Terminal(e, true) with BinaryReader
  with WithParseErrorThrowing {

  def parser : Parser = new PrimParser(this, e) {

    override def toBriefXML(depthLimit : Int = -1) = {
      "<StringFixedLengthInBytesParser length='" + nBytes + "'/>"
    }

    override def toString = toBriefXML()

    val decoder = e.knownEncodingDecoder

    def parse(start : PState) : PState = withParseErrorThrowing(start) {
      withLoggingLevel(LogLevel.Info) {
        log(Debug("StringFixedLengthInBytes - Saving reader state."))
        setReader(start)

        log(Debug("StringFixedLengthInBytes - Parsing starting at bit position: %s", start.bitPos))

        if (start.bitPos % 8 != 0) { return PE(start, "StringFixedLengthInBytes - not byte aligned.") }

        val in : InStreamFromByteChannel = start.inStream.asInstanceOf[InStreamFromByteChannel]

        val bytePos = (start.bitPos >> 3).toInt

        try {
          val bytes = in.getBytes(start.bitPos, nBytes.toInt)
          val cb = decoder.decode(ByteBuffer.wrap(bytes))
          val result = cb.toString
          val endBitPos = start.bitPos + (nBytes.toInt * 8)
          log(Debug("Parsed: %s", result))
          log(Debug("Ended at bit position %s", endBitPos))
          val endCharPos = start.charPos + result.length
          val currentElement = start.parentForAddContent
          // Assert.invariant(currentElement.getName != "_document_")
          // Note: this side effect is backtracked, because at points of uncertainty, pre-copies of a node are made
          // and when backtracking occurs they are used to replace the nodes modified by sub-parsers.
          currentElement.addContent(new org.jdom.Text(result))
          //val postState = start.withPos(endBitPos, endCharPos)
          val postState = start.withPos(endBitPos, -1) // -1 means a subsequent primitive will have to construct
          // a new reader at said bitPosition
          return postState
        } catch {
          case e : java.nio.BufferUnderflowException => { return PE(start, "StringFixedLengthInBytes - Insufficient Bits in field; required %s", nBytes * 8) }
          case e : IndexOutOfBoundsException => { return PE(start, "StringFixedLengthInBytes - IndexOutOfBounds: \n%s", e.getMessage()) }
          case u : UnsuppressableException => throw u
          case e : Exception => { return PE(start, "StringFixedLengthInBytes - Exception: \n%s", e.getMessage()) }
        }
      }
    }
  }

  def unparser : Unparser = new Unparser(e) {
    override def toString = "StringFixedLengthInBytesUnparser(" + nBytes + ")"
    val encoder = e.knownEncodingEncoder

    def unparse(start : UState) : UState = {
      // setLoggingLevel(LogLevel.Info)

      val data = start.currentElement.getText

      start.outStream.setEncoder(encoder)
      start.outStream.fillCharBuffer(data)

      log(Debug("Unparsed: " + start.outStream.getData))
      start
    }
  }
}

case class StringFixedLengthInBytesVariableWidthCharacters(e : ElementBase, nBytes : Long)
  extends Terminal(e, true)
  with WithParseErrorThrowing with BinaryReader {

  def parser : Parser = new PrimParser(this, e) {
    override def toString = "StringFixedLengthInBytesVariableWidthCharactersParser(" + nBytes + ")"
    val decoder = e.knownEncodingDecoder

    def parse(start : PState) : PState = withParseErrorThrowing(start) {
      withLoggingLevel(LogLevel.Info) {
        log(Debug("Saving Reader state."))
        setReader(start)

        log(Debug("Parsing starting at bit position: %s", start.bitPos))

        if (start.bitPos % 8 != 0) { return PE(start, "StringFixedLengthInBytesVariableWidthCharacters - not byte aligned.") }

        val in : InStreamFromByteChannel = start.inStream.asInstanceOf[InStreamFromByteChannel]

        val bytePos = (start.bitPos >> 3).toInt

        val byteReader = in.byteReader.atPos(bytePos)

        try {
          val bytes = in.getBytes(start.bitPos, nBytes.toInt)
          val cb = decoder.decode(ByteBuffer.wrap(bytes))
          val result = cb.toString
          val endBitPos = start.bitPos + (nBytes.toInt * 8)
          log(Debug("Parsed: %s", result))
          log(Debug("Ended at bit position %s", endBitPos))
          val endCharPos = start.charPos + result.length
          val currentElement = start.parentForAddContent
          // Assert.invariant(currentElement.getName != "_document_")
          // Note: this side effect is backtracked, because at points of uncertainty, pre-copies of a node are made
          // and when backtracking occurs they are used to replace the nodes modified by sub-parsers.
          currentElement.addContent(new org.jdom.Text(result))
          //val postState = start.withPos(endBitPos, endCharPos)
          val postState = start.withPos(endBitPos, -1) // charPos = -1 tells subsequent calls to construct
          // a new reader at said bitPos
          return postState
        } catch {
          case e : java.nio.BufferUnderflowException => { return PE(start, "StringFixedLengthInBytesVariableWidthCharacters - Insufficient Bits in field; required %s", nBytes * 8) }
          case e : IndexOutOfBoundsException => { return PE(start, "StringFixedLengthInBytesVariableWidthCharacters - IndexOutOfBoundsException: \n%s", e.getMessage()) }
          case u : UnsuppressableException => throw u
          case e : Exception => { return PE(start, "StringFixedLengthInBytesVariableWidthCharacters - Exception: \n%s", e.getMessage()) }
        }
        //      // setLoggingLevel(LogLevel.Info)
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

  def unparser : Unparser = new Unparser(e) {
    def unparse(start : UState) : UState = {
      Assert.notYetImplemented()
    }
  }
}

case class StringFixedLengthInVariableWidthCharacters(e : ElementBase, nChars : Long)
  extends Terminal(e, true)
  with WithParseErrorThrowing with TextReader {

  def parser : Parser = new PrimParser(this, e) {
    override def toString = "StringFixedLengthInVariableWidthCharactersParser(" + nChars + ")"
    val decoder = e.knownEncodingDecoder

    def parse(start : PState) : PState = withParseErrorThrowing(start) {
      withLoggingLevel(LogLevel.Info) {

        log(Debug("Parsing starting at bit position: %s", start.bitPos))

        if (start.bitPos % 8 != 0) { return PE(start, "StringFixedLengthInVariableWidthCharacters - not byte aligned.") }

        val in : InStreamFromByteChannel = start.inStream.asInstanceOf[InStreamFromByteChannel]

        val bytePos = (start.bitPos >> 3).toInt

        log(Debug("Retrieving reader"))

        val reader = getReader(bytePos, decoder.charset().name(), start)

        //        val byteReader = in.byteReader.atPos(bytePos)
        //
        //        val reader = byteReader.charReader(decoder.charset().name())

        val d = new delimsearch.DelimParser()

        val result = d.parseInputNCharacters(nChars, reader, decoder.charset())

        if (!result.isSuccess) {
          return PE(start, "Parse failed to find exactly %s characters.", nChars)
        }

        val parsedField = result.field
        val parsedBytes = result.numBytes
        val endBitPos = start.bitPos + (parsedBytes * 8)

        log(Debug("Parsed: %s", parsedField))
        log(Debug("Ended at bit position: %s", endBitPos))

        //val endCharPos = start.charPos + nChars
        //val endCharPos = reader.characterPos + nChars
        val endCharPos = if (start.charPos == -1) nChars else start.charPos + nChars
        val currentElement = start.parentForAddContent
        currentElement.addContent(new org.jdom.Text(parsedField))
        //        val postState = start.withPos(endBitPos, endCharPos)
        val postState = start.withReaderPos(endBitPos, endCharPos, reader)
        postState

        //      //setLoggingLevel(LogLevel.Info)
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

  def unparser : Unparser = new Unparser(e) {
    def unparse(start : UState) : UState = {
      Assert.notYetImplemented()
    }
  }
}

case class StringDelimitedEndOfData(e : ElementBase)
  extends Terminal(e, true)
  with WithParseErrorThrowing with TextReader {
  lazy val es = e.escapeScheme
  lazy val esObj = EscapeScheme.getEscapeScheme(es, e)
  lazy val tm = e.allTerminatingMarkup
  lazy val cname = toString

  def parser : Parser = new PrimParser(this, e) {
    override def toString = cname + "(" + tm.map { _.prettyExpr } + ")"
    val decoder = e.knownEncodingDecoder

    def parse(start : PState) : PState = withParseErrorThrowing(start) {
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
        //System.err.println("startCharPos: " + start.charPos)
        val postEvalState = start.withVariables(vars)

        if (delimsCooked.filter(x => x == "%WSP*;").length > 0) {
          // We cannot detect this error until expressions have been evaluated!
          log(Debug("%s - Failed due to WSP* detected as a delimiter for lengthKind=delimited", eName))
          e.schemaDefinitionError("WSP* cannot be used as a delimiter when lengthKind=delimited!")
        }

        log(Debug("%s - Looking for: %s Count: %s", eName, delimsCooked, delimsCooked.length))
        val in : InStreamFromByteChannel = start.inStream.asInstanceOf[InStreamFromByteChannel]

        val bytePos = (postEvalState.bitPos >> 3).toInt
        log(Debug("%s - Starting at bit pos: %s", eName, postEvalState.bitPos))
        log(Debug("%s - Starting at byte pos: %s", eName, bytePos))

        //System.err.println("StringDelimitedEndOfData_START: " + new Timestamp(System.currentTimeMillis()));

        if (postEvalState.bitPos % 8 != 0) {
          //System.err.println("StringDelimitedEndOfData_END: " + new Timestamp(System.currentTimeMillis()));
          return PE(postEvalState, "StringDelimitedEndOfData - not byte aligned.")
        }

        log(Debug("Retrieving reader"))

        val reader = getReader(bytePos, decoder.charset().name(), start)

        //System.err.println("StringDelimitedEndOfData - " + reader.characterPos)

        //        val byteReader = in.byteReader.atPos(bytePos)
        //        val reader = byteReader.charReader(decoder.charset().name())

        val d = new delimsearch.DelimParser()

        var result : delimsearch.DelimParseResult = new delimsearch.DelimParseResult

        //        val remaining = in.getBytesRemaining(start.bitPos)
        //        
        //        //System.err.println(remaining.toList)

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
          //System.err.println("StringDelimitedEndOfData_END: " + new Timestamp(System.currentTimeMillis()));
          return PE(postEvalState, "%s - %s - Parse failed.", this.toString(), eName)
        } else {
          //System.err.println("StringDelimitedEndOfData_END: " + new Timestamp(System.currentTimeMillis()));
          val field = result.field
          val numBytes = result.numBytes
          log(Debug("%s - Parsed: %s Parsed Bytes: %s", eName, field, numBytes))
          //System.err.println(postEvalState.charPos)
          //val endCharPos = reader.characterPos + field.length()
          val endCharPos = if (postEvalState.charPos == -1) result.numCharsRead else postEvalState.charPos + result.numCharsRead
          //val endCharPos = postEvalState.charPos + field.length()
          val endBitPos = postEvalState.bitPos + (numBytes * 8)
          val currentElement = postEvalState.parentForAddContent
          currentElement.addContent(new org.jdom.Text(field))
          //return postEvalState.withPos(endBitPos, endCharPos)
          return postEvalState.withReaderPos(endBitPos, endCharPos, reader)
        }
      }
    }
  }

  def unparser : Unparser = new Unparser(e) {
    override def toString = cname + "(" + tm.map { _.prettyExpr } + ")"
    val encoder = e.knownEncodingEncoder

    def unparse(start : UState) : UState = {
      // setLoggingLevel(LogLevel.Info)

      val data = start.currentElement.getText

      start.outStream.setEncoder(encoder)
      start.outStream.fillCharBuffer(data)
      log(Debug("Unparsed: " + start.outStream.getData))
      start
    }
  }
}

case class StringPatternMatched(e : ElementBase)
  extends Terminal(e, true)
  with WithParseErrorThrowing with TextReader {

  def parser : Parser = new PrimParser(this, e) {
    override def toString = "StringPatternMatched"
    val decoder = e.knownEncodingDecoder
    val pattern = e.lengthPattern

    // TODO: Add parameter for changing CharBuffer size

    def parse(start : PState) : PState = withParseErrorThrowing(start) {
      withLoggingLevel(LogLevel.Info) {
        val eName = e.toString()

        log(Debug("StringPatternMatched - %s - Parsing pattern at byte position: %s", eName, (start.bitPos >> 3)))
        log(Debug("StringPatternMatched - %s - Parsing pattern at bit position: %s", eName, start.bitPos))

        if (start.bitPos % 8 != 0) { return PE(start, "StringPatternMatched - not byte aligned.") }

        val in : InStreamFromByteChannel = start.inStream.asInstanceOf[InStreamFromByteChannel]

        val bytePos = (start.bitPos >> 3).toInt

        log(Debug("Retrieving reader"))

        val reader = getReader(bytePos, decoder.charset().name(), start)

        //        val byteReader = in.byteReader.atPos(bytePos)
        //        val reader = byteReader.charReader(decoder.charset().name())

        val d = new delimsearch.DelimParser()

        var result : delimsearch.DelimParseResult = new delimsearch.DelimParseResult

        result = d.parseInputPatterned(pattern, reader, decoder.charset())

        val postState = result.isSuccess match {
          case true => {
            val endBitPos = start.bitPos + (result.numBytes * 8)
            log(Debug("StringPatternMatched - Parsed: %s", result.field))
            log(Debug("StringPatternMatched - Ended at bit position %s", endBitPos))
            // val endCharPos = start.charPos + result.field.length()
            //val endCharPos = reader.characterPos + result.field.length()
            val endCharPos = if (start.charPos == -1) result.field.length() else start.charPos + result.field.length()
            val currentElement = start.parentForAddContent
            currentElement.addContent(new org.jdom.Text(result.field))
            //start.withPos(endBitPos, endCharPos)
            start.withReaderPos(endBitPos, endCharPos, reader)
          }
          case false => { PE(start, "%s: No match found!", this.toString()) }
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

  def unparser : Unparser = new Unparser(e) {
    def unparse(start : UState) : UState = {
      Assert.notYetImplemented()
    }
  }
}

abstract class ConvertTextNumberPrim[S](e : ElementBase, guard : Boolean)
  extends Terminal(e, guard) with BinaryReader {
  protected def getNum(s : Number) : S
  protected val GramName = "number"
  protected val GramDescription = "Number"
  override def toString = "to(xs:" + GramName + ")"

  protected def numFormat : NumberFormat
  protected def isInt : Boolean

  protected def isInvalidRange(n : java.lang.Number) : Boolean

  protected def getStringFormat(n : S) : String

  def parser : Parser = new PrimParser(this, e) {
    override def toString = "to(xs:" + GramName + ")"

    def parse(start : PState) : PState = {
      log(Debug("Saving reader state."))
      setReader(start)

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
          case u : UnsuppressableException => throw u
          case e : Exception =>
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

        // The following change was made because of the issues with the float
        // adding a position of precision to the Number object.  At some point we
        // will want to revert this back to the actual type but this is a quick fix
        // for the issues we were having with the 0.003 vs 0.0030 error in test_DelimProp_05
        //
        //node.setText(asNumber.toString)
        val result = getStringFormat(asNumber)
        node.setText(result)

        start
      } // catch { case e: Exception => start.failed("Failed to convert %s to an xs:%s" + GramName) }

      resultState
    }
  }

  // TODO: consolidate duplicate code
  def unparser : Unparser = new Unparser(e) {
    override def toString = "to(xs:" + GramName + ")"

    /*
      * Converts data to number format, returns unparse exception if data cannot be converted to given format.
      */
    def unparse(start : UState) : UState = {
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
        case u : UnsuppressableException => throw u
        case e : Exception =>
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

abstract class ConvertTextIntegerNumberPrim[T](e : ElementBase, g : Boolean)
  extends ConvertTextNumberPrim[T](e, g) {
  override def numFormat = NumberFormat.getIntegerInstance()
  override def isInt = true

  protected override def getStringFormat(n : T) : String = n.toString()

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
  def min : Long
  def max : Long
  private def isInvalidRange(l : Long) = {
    l < min || l > max
  }
}

abstract class ConvertTextFloatingPointNumberPrim[T](e : ElementBase, g : Boolean)
  extends ConvertTextNumberPrim[T](e, g) {
  override def numFormat = NumberFormat.getNumberInstance() // .getScientificInstance() Note: scientific doesn't allow commas as grouping separators.
  override def isInt = false
  protected override def getStringFormat(n : T) : String = {
    //val trailingZeroes = """0*(?!<[1-9])$"""
    val trailingZeroes = """(?<=[1-9])(0*)$""".r
    val trailingZeroesBeforeExponent = """(?<=[1-9])(0*?)(?=E.*)""".r

    val nAsStr = n.toString()

    if (nAsStr.contains("E") || nAsStr.contains("e")) {
      // Exponent
      return trailingZeroesBeforeExponent.replaceAllIn(nAsStr, "")
    } else {
      return trailingZeroes.replaceAllIn(nAsStr, "")
    }

    nAsStr
  }
}

case class ConvertTextIntegerPrim(e : ElementBase) extends ConvertTextIntegerNumberPrim[BigInteger](e, true) {
  protected override def getNum(num : Number) = new BigInteger(num.toString)
  protected override val GramName = "integer"
  protected override val GramDescription = "Unbounded Integer"
  protected override def isInvalidRange(n : java.lang.Number) : Boolean = false
  def min = -1 // ignored
  def max = -1 // ignored
}

case class ConvertTextLongPrim(e : ElementBase) extends ConvertTextIntegerNumberPrim[Long](e, true) {
  protected override def getNum(num : Number) = num.longValue
  protected override val GramName = "long"
  protected override val GramDescription = "Long Integer"
  val min = Long.MinValue
  val max = Long.MaxValue
}

case class ConvertTextIntPrim(e : ElementBase) extends ConvertTextIntegerNumberPrim[Int](e, true) {
  protected override def getNum(num : Number) = num.intValue
  protected override val GramName = "int"
  protected override val GramDescription = "Integer"
  val min = Int.MinValue.toLong
  val max = Int.MaxValue.toLong
}

case class ConvertTextShortPrim(e : ElementBase) extends ConvertTextIntegerNumberPrim[Short](e, true) {
  protected override def getNum(num : Number) = num.shortValue
  protected override val GramName = "short"
  protected override val GramDescription = "Short Integer"
  val min = Short.MinValue.toLong
  val max = Short.MaxValue.toLong
}

case class ConvertTextBytePrim(e : ElementBase) extends ConvertTextIntegerNumberPrim[Byte](e, true) {
  protected override def getNum(num : Number) = num.byteValue
  protected override val GramName = "byte"
  protected override val GramDescription = "Byte"
  val min = Byte.MinValue.toLong
  val max = Byte.MaxValue.toLong
}

case class ConvertTextUnsignedLongPrim(e : ElementBase) extends ConvertTextIntegerNumberPrim[BigInteger](e, true) {
  protected override def getNum(num : Number) = new BigInteger(num.toString)
  protected override val GramName = "unsignedLong"
  protected override val GramDescription = "Unsigned Long"
  protected override def isInvalidRange(jn : java.lang.Number) = {
    jn match {
      case n : BigInteger => {
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

case class ConvertTextUnsignedIntPrim(e : ElementBase) extends ConvertTextIntegerNumberPrim[Long](e, true) {
  protected override def getNum(num : Number) = num.longValue
  protected override val GramName = "unsignedInt"
  protected override val GramDescription = "Unsigned Int"
  val min = 0L
  val max = (1L << 32) - 1L
}
// TODO: Restore leading '+' sign and leading/trailing 0's
case class ConvertTextUnsignedShortPrim(e : ElementBase) extends ConvertTextIntegerNumberPrim[Int](e, true) {
  protected override def getNum(num : Number) = num.intValue
  protected override val GramName = "unsignedShort"
  protected override val GramDescription = "Unsigned Short"
  val min = 0L
  val max = (1L << 16) - 1L
}

case class ConvertTextUnsignedBytePrim(e : ElementBase) extends ConvertTextIntegerNumberPrim[Short](e, true) {
  protected override def getNum(num : Number) = num.shortValue
  protected override val GramName = "unsignedByte"
  protected override val GramDescription = "Unsigned Byte"
  val min = 0L
  val max = (1L << 8) - 1L
}

case class ConvertTextDoublePrim(e : ElementBase) extends ConvertTextFloatingPointNumberPrim[Double](e, true) {
  protected override def getNum(num : Number) = num.doubleValue
  protected override val GramName = "double"
  protected override val GramDescription = "Double"
  protected def isInvalidRange(n : java.lang.Number) : Boolean = false
}

case class ConvertTextFloatPrim(e : ElementBase) extends ConvertTextFloatingPointNumberPrim[Float](e, true) {
  protected override def getNum(num : Number) = num.floatValue
  protected override val GramName = "float"
  protected override val GramDescription = "Float"
  protected def isInvalidRange(n : java.lang.Number) : Boolean = {
    if (n == null) return false // tolerate null here. We catch that error elsewhere.
    val d = n.doubleValue()
    if (d.isNaN) false
    else (d < Float.MinValue) || d > Float.MaxValue
  }

}

//Is this different from ConvertTextNumberPrim?
//case class ConvertTextDoublePrim1(e: ElementBase) extends Terminal(e, true) {
//
//  def parser: Parser = new PrimParser(this, e) {
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
//  def parser: Parser = new PrimParser(this, e) {
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

abstract class Primitive(e : AnnotatedSchemaComponent, guard : Boolean = false)
  extends Terminal(e, guard) {
  override def toString = "Prim[" + name + "]"
  def parser : Parser = DummyParser(e)
  def unparser : Unparser = DummyUnparser(e)
}

abstract class ZonedTextNumberPrim(e : ElementBase, guard : Boolean) extends Terminal(e, guard) {
  def parser : Parser = new PrimParser(this, e) {
    def parse(start : PState) : PState = {
      Assert.notYetImplemented()
    }
  }

  def unparser : Unparser = new Unparser(e) {
    def unparse(start : UState) : UState = {
      Assert.notYetImplemented()
    }
  }
}
case class ZonedTextBytePrim(el : ElementBase) extends ZonedTextNumberPrim(el, false)
case class ZonedTextShortPrim(el : ElementBase) extends ZonedTextNumberPrim(el, false)
case class ZonedTextIntPrim(el : ElementBase) extends ZonedTextNumberPrim(el, false)
case class ZonedTextLongPrim(el : ElementBase) extends ZonedTextNumberPrim(el, false)

abstract class BinaryNumber[T](e : ElementBase, nBits : Long) extends Terminal(e, true) with BinaryReader {
  lazy val primName = e.primType.name

  lazy val staticByteOrderString = e.byteOrder.constantAsString
  lazy val staticByteOrder = ByteOrder(staticByteOrderString, context)

  lazy val (staticJByteOrder, label) = staticByteOrder match {
    case ByteOrder.BigEndian => (java.nio.ByteOrder.BIG_ENDIAN, "BE")
    case ByteOrder.LittleEndian => (java.nio.ByteOrder.LITTLE_ENDIAN, "LE")
  }

  def getNum(bitPos : Long, inStream : InStream, byteOrder : java.nio.ByteOrder) : T
  def getNum(t : Number) : T
  override def toString = "binary(xs:" + primName + ", " + label + ")"
  val gram = this

  protected val GramName = "binary"
  protected val GramDescription = "Binary"
  protected def numFormat : NumberFormat
  protected def isInt : Boolean
  protected def isInvalidRange(n : T) : Boolean = false

  def parser = new PrimParser(this, e) {
    override def toString = gram.toString

    def parse(start : PState) : PState = {
      log(Debug("Saving reader state."))
      setReader(start)

      if (start.bitLimit != -1L && (start.bitLimit - start.bitPos < nBits)) PE(start, "Not enough bits to create an xs:%s", primName)
      else {
        val value = getNum(start.bitPos, start.inStream, staticJByteOrder)
        if (GramName == "hexBinary") {
          val bytes = value.asInstanceOf[Array[Byte]]
          var asString : StringBuilder = new StringBuilder()
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

    def unparse(start : UState) : UState = {
      setLoggingLevel(LogLevel.Info)
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
            case u : UnsuppressableException => throw u
            case e : Exception =>
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

case class PackedIntPrim(e : ElementBase) extends Primitive(e, false)
case class BCDIntPrim(e : ElementBase) extends Primitive(e, false)

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

case class FloatPrim(gram : Gram, ctx : Term, byteOrder : java.nio.ByteOrder) extends PrimParser(gram, ctx) with BinaryReader {
  override def toString = "binary(xs:float,you " + byteOrder + ")"

  def parse(start : PState) : PState = {
    log(Debug("Saving reader state."))
    setReader(start)

    if (start.bitLimit != -1L && (start.bitLimit - start.bitPos < 32)) PE(start, "Not enough bits to create an xs:float")
    else {
      val value = start.inStream.getFloat(start.bitPos, byteOrder)
      start.parentForAddContent.addContent(new org.jdom.Text(value.toString))
      val postState = start.withPos(start.bitPos + 32, -1)
      postState
    }
  }
}

case class FloatPrimUnparse(ctx : Term, byteOrder : java.nio.ByteOrder) extends Unparser(ctx) {
  override def toString = "binary(xs:float, " + byteOrder + ")"

  def unparse(start : UState) : UState = {
    Assert.notYetImplemented()
  }
}

case class BigEndianFloatPrim(e : ElementBase) extends Terminal(e, true) {
  def parser = new FloatPrim(this, e, java.nio.ByteOrder.BIG_ENDIAN)
  def unparser = new FloatPrimUnparse(e, java.nio.ByteOrder.BIG_ENDIAN)
}

case class LittleEndianFloatPrim(e : ElementBase) extends Terminal(e, true) {
  def parser = new FloatPrim(this, e, java.nio.ByteOrder.LITTLE_ENDIAN)
  def unparser = new FloatPrimUnparse(e, java.nio.ByteOrder.LITTLE_ENDIAN)
}

abstract class StaticDelimiter(kindString : String, delim : String, e : Term, guard : Boolean = true)
  extends StaticText(delim, e, kindString, guard)

abstract class StaticText(delim : String, e : Term, kindString : String, guard : Boolean = true)
  extends Terminal(e, guard)
  with WithParseErrorThrowing with TextReader {

  lazy val es = e.escapeScheme
  lazy val esObj = EscapeScheme.getEscapeScheme(es, e)

  val term = e.asInstanceOf[Term]
  lazy val staticTexts = delim.split("\\s").toList

  val staticTextsCooked : Queue[String] = new Queue

  staticTexts.foreach(x => staticTextsCooked.enqueue(EntityReplacer.replaceAll(x)))

  def parser : Parser = new PrimParser(this, e) {

    override def toBriefXML(depthLimit : Int = -1) = {
      "<" + kindString + ">" + delim + "</" + kindString + ">"
    }

    Assert.notYetImplemented(e.ignoreCase == YesNo.Yes)

    Assert.invariant(delim != "") // shouldn't be here at all in this case.
    override def toString = kindString + "('" + delim + "')" //  with terminating markup: " + term.prettyTerminatingMarkup + ")"
    val decoder = e.knownEncodingDecoder

    def parse(start : PState) : PState = withParseErrorThrowing(start) {
      withLoggingLevel(LogLevel.Info) {
        val eName = e.toString()

        log(Debug("%s - Parsing delimiter at byte position: %s", eName, (start.bitPos >> 3)))
        log(Debug("%s - Parsing delimiter at bit position: %s", eName, start.bitPos))

        if (start.bitPos % 8 != 0) {
          return PE(start, "%s - not byte aligned.", kindString)
        }

        val in : InStreamFromByteChannel = start.inStream.asInstanceOf[InStreamFromByteChannel]

        val bytePos = (start.bitPos >> 3).toInt

        //System.err.println("PState.charPos = " + start.charPos)
        log(Debug("Retrieving reader state."))
        val reader = getReader(bytePos, decoder.charset().name(), start)

        //System.err.println("StaticText - " + reader.characterPos)

        //        val byteReader = in.byteReader.atPos(bytePos)
        //        val reader = byteReader.charReader(decoder.charset().name())

        val d = new delimsearch.DelimParser()

        var result : delimsearch.DelimParseResult = new delimsearch.DelimParseResult

        // Well they may not be delimiters, but the logic is the same as for a 
        // set of static delimiters.
        result = d.parseInputDelimiter(staticTextsCooked.toSet, reader, decoder.charset())

        if (!result.isSuccess) {
          return PE(start, "%s - %s: Delimiter not found!", this.toString(), eName)
        } else {
          val numBytes = result.delimiter.getBytes(decoder.charset()).length
          //val endCharPos = start.charPos + result.field.length()
          //System.err.println(reader.characterPos + "-" + result.delimiter.length())
          //val endCharPos = reader.characterPos + result.delimiter.length
          val endCharPos = if (start.charPos == -1) result.delimiter.length else start.charPos + result.delimiter.length()
          val endBitPosDelim = (8 * numBytes) + start.bitPos

          log(Debug("%s - Found %s", eName, result.delimiter))
          log(Debug("%s - Ended at byte position %s", eName, (endBitPosDelim >> 3)))
          log(Debug("%s - Ended at bit position %s", eName, endBitPosDelim))

          // return start.withPos(endBitPosDelim, endCharPos)
          return start.withReaderPos(endBitPosDelim, endCharPos, reader)
        }
        start
      }
    }
  }

  def unparser : Unparser = new Unparser(e) {
    val t = e.asInstanceOf[Term]
    override def toString = "StaticText('" + delim + "' with terminating markup: " + t.prettyTerminatingMarkup + ")"
    // setLoggingLevel(LogLevel.Info)
    Assert.notYetImplemented(e.ignoreCase == YesNo.Yes)
    Assert.invariant(delim != "") //shouldn't be here at all in this case

    def unparse(start : UState) : UState = {
      val encoder = e.knownEncodingEncoder
      start.outStream.setEncoder(encoder)
      start.outStream.fillCharBuffer(unparserDelim)
      log(Debug("Unparsed: " + start.outStream.getData))
      start
    }
  }

  def unparserDelim : String
}

class DynamicDelimiter(delimExpr : CompiledExpression, e : Term, guard : Boolean = true) extends Primitive(e, guard)

//case class StaticInitiator(e: Term) extends StaticDelimiter(e.initiator.constantAsString, e)
case class StaticInitiator(e : Term) extends StaticDelimiter("Init", e.initiator.constantAsString, e) {
  Assert.invariant(e.hasInitiator)
  lazy val unparserDelim = e.initiator.constantAsString.split("""\s""").head
}
//case class StaticTerminator(e : Term) extends StaticDelimiter(e.terminator.constantAsString, e)
case class StaticTerminator(e : Term) extends StaticDelimiter("Term", e.terminator.constantAsString, e) {
  Assert.invariant(e.hasTerminator)
  lazy val unparserDelim = e.terminator.constantAsString.split("""\s""").head
}
case class DynamicInitiator(e : Term) extends DynamicDelimiter(e.initiator, e)
case class DynamicTerminator(e : Term) extends DynamicDelimiter(e.terminator, e)

case class StaticSeparator(s : Sequence, t : Term) extends StaticDelimiter("Sep", s.separator.constantAsString, t) {
  Assert.invariant(s.hasSeparator)
  lazy val unparserDelim = s.separator.constantAsString.split("""\s""").head
}
case class DynamicSeparator(s : Sequence, t : Term) extends DynamicDelimiter(s.separator, t)

case class StartChildren(ct : ComplexTypeBase, guard : Boolean = true) extends Terminal(ct.element, guard) {

  def parser : Parser = new PrimParser(this, ct.element) {
    override def toString = "StartChildren"

    def parse(start : PState) : PState = {
      val postState = start.withChildIndexStack(1L :: start.childIndexStack)
      postState
    }
  }

  def unparser : Unparser = new Unparser(ct.element) {
    override def toString = "StartChildren"

    def unparse(start : UState) : UState = {
      val postState = start.withChildIndexStack(1L :: start.childIndexStack)
      postState
    }
  }
}

case class StartSequence(sq : Sequence, guard : Boolean = true) extends Terminal(sq, guard) {

  def parser : Parser = new PrimParser(this, sq) {
    override def toString = "StartSequence"

    def parse(start : PState) : PState = {
      val postState = start.withGroupIndexStack(1L :: start.groupIndexStack)
      postState
    }
  }

  def unparser : Unparser = new Unparser(sq) {
    override def toString = "StartSequence"

    def unparse(start : UState) : UState = {
      val postState = start.withGroupIndexStack(1L :: start.groupIndexStack)
      postState
    }
  }
}

case class Nada(sc : Term) extends Terminal(sc, true) {
  override def isEmpty = false
  // cannot optimize this out! It is used as an alternative to things
  // with the intention of "find this and this, or find nothing"

  def parser : Parser = new PrimParser(this, sc) {
    override def toString = "Nada"

    def parse(start : PState) : PState = start
  }

  def unparser : Unparser = new Unparser(sc) {
    override def toString = "Nada"

    def unparse(start : UState) : UState = start
  }
}

case class GroupPosGreaterThan(groupPos : Long, term : Term, guard : Boolean = true) extends Terminal(term, guard) {

  def parser : Parser = new PrimParser(this, term) {
    override def toString = "GroupPosGreaterThan(" + groupPos + ")"

    def parse(start : PState) : PState = {
      val res = if (start.groupPos > groupPos) {
        start.withDiscriminator(true)
      } else {
        PE(start, "Group position not greater than (%s)", groupPos)
      }
      res
    }
  }

  def unparser : Unparser = new Unparser(term) {
    override def toString = "GroupPosGreaterThan(" + groupPos + ")"

    def unparse(start : UState) : UState = {
      val res = if (start.groupPos > groupPos) {
        start.withDiscriminator(true)
      } else {
        UE(start, "Group position not greater than (%s)", groupPos)
      }
      res
    }
  }
}

case class ChildPosGreaterThan(childPos : Long, term : Term, guard : Boolean = true) extends Terminal(term, guard) {

  def parser : Parser = new PrimParser(this, term) {
    override def toString = "ChildPosGreaterThan(" + childPos + ")"

    def parse(start : PState) : PState = {
      val res = if (start.childPos > childPos) {
        start.withDiscriminator(true)
      } else {
        PE(start, "Child position not greater than (%s)", childPos)
      }
      res
    }
  }

  def unparser : Unparser = new Unparser(term) {
    override def toString = "ChildPosGreaterThan(" + childPos + ")"

    def unparse(start : UState) : UState = {
      val res = if (start.childPos > childPos) {
        start.withDiscriminator(true)
      } else {
        UE(start, "Child position not greater than (%s)", childPos)
      }
      res
    }
  }
}

case class ArrayPosGreaterThan(arrayPos : Long, term : Term, guard : Boolean = true) extends Terminal(term, guard) {

  def parser : Parser = new PrimParser(this, term) {
    override def toString = "ArrayPosGreaterThan(" + arrayPos + ")"

    def parse(start : PState) : PState = {
      val res = try {
        if (start.arrayPos > arrayPos) {
          start.withDiscriminator(true)
        } else {
          PE(start, "Array position not greater than (%s)", arrayPos)
        }
      } catch { case e => PE(start, "No array position") }
      res
    }
  }

  def unparser : Unparser = new Unparser(term) {
    override def toString = "ArrayPosGreaterThan(" + arrayPos + ")"

    def unparse(start : UState) : UState = {
      val res = try {
        if (start.arrayPos > arrayPos) {
          start.withDiscriminator(true)
        } else {
          UE(start, "Array position not greater than (%s)", arrayPos)
        }
      } catch { case e => UE(start, "No array position") }
      res
    }
  }
}

case class EndChildren(ct : ComplexTypeBase, guard : Boolean = true) extends Terminal(ct.element, guard) {

  def parser : Parser = new PrimParser(this, ct.element) {
    override def toString = "EndChildren"

    def parse(start : PState) : PState = {
      val postState = start.withChildIndexStack(start.childIndexStack.tail)
      postState
    }
  }

  def unparser : Unparser = new Unparser(ct.element) {
    override def toString = "EndChildren"

    def unparse(start : UState) : UState = {
      val postState = start.withChildIndexStack(start.childIndexStack.tail)
      postState
    }
  }
}

case class EndSequence(sq : Sequence, guard : Boolean = true) extends Terminal(sq, guard) {

  def parser : Parser = new PrimParser(this, sq) {
    override def toString = "EndSequence"

    def parse(start : PState) : PState = {
      val postState = start.withGroupIndexStack(start.groupIndexStack.tail).moveOverOneGroupIndexOnly
      postState
    }
  }

  def unparser : Unparser = new Unparser(sq) {
    override def toString = "EndSequence"

    def unparse(start : UState) : UState = {
      val postState = start.withGroupIndexStack(start.groupIndexStack.tail).moveOverOneGroupIndexOnly
      postState
    }
  }
}

case class StartArray(e : ElementBase, guard : Boolean = true) extends Terminal(e, guard) {

  def parser : Parser = new PrimParser(this, e) {
    override def toString = "StartArray"

    def parse(start : PState) : PState = {
      val postState1 = start.withArrayIndexStack(1L :: start.arrayIndexStack)
      val postState2 = postState1.withOccursCountStack(Compiler.occursCountMax :: postState1.occursCountStack)
      postState2
    }
  }

  def unparser : Unparser = new Unparser(e) {
    override def toString = "StartArray"

    def unparse(start : UState) : UState = {
      val postState = start.withArrayIndexStack(1L :: start.arrayIndexStack)
      postState
    }
  }
}

case class EndArray(e : ElementBase, guard : Boolean = true) extends Terminal(e, guard) {

  def parser : Parser = new PrimParser(this, e) {
    override def toString = "EndArray"

    def parse(start : PState) : PState = {
      val postState1 = start.withArrayIndexStack(start.arrayIndexStack.tail)
      val postState2 = postState1.withOccursCountStack(postState1.occursCountStack.tail)
      postState2
    }
  }

  def unparser : Unparser = new Unparser(e) {
    override def toString = "EndArray"

    def unparse(start : UState) : UState = {
      val postState = start.withArrayIndexStack(start.arrayIndexStack.tail)
      postState
    }
  }
}

case class NoValue(e : GlobalElementDecl, guard : Boolean = true) extends Primitive(e, guard)

case class SaveInputStream(e : ElementBase, guard : Boolean = true) extends Primitive(e, guard)

case class SetEmptyInputStream(e : ElementBase, guard : Boolean = true) extends Primitive(e, guard)

case class RestoreInputStream(e : ElementBase, guard : Boolean = true) extends Primitive(e, guard)

//case class Value(e: SchemaComponent, guard: Boolean = true) extends Primitive(e, guard) 

case class NotStopValue(e : ElementBase with LocalElementMixin) extends Primitive(e, e.hasStopValue)

case class StopValue(e : ElementBase with LocalElementMixin) extends Primitive(e, e.hasStopValue)

case class TheDefaultValue(e : ElementBase) extends Primitive(e, e.isDefaultable)

case class LiteralNilPattern(e : ElementBase)
  extends StaticText(e.nilValue, e, "LiteralNilPattern", e.isNillable) {
  lazy val unparserDelim = Assert.notYetImplemented()
  //val stParser = super.parser

  override def parser = new PrimParser(this, e) {
    override def toString = "LiteralNilPattern(" + e.nilValue + ")"
    val decoder = e.knownEncodingDecoder
    val pattern = e.lengthPattern

    def parse(start : PState) : PState = {
      withLoggingLevel(LogLevel.Info) {
        val eName = e.toString()

        val nilValuesCooked = new daffodil.dsom.ListOfStringValueAsLiteral(e.nilValue, e).cooked
        val postEvalState = start //start.withVariables(vars)

        log(Debug("%s - Looking for: %s Count: %s", eName, nilValuesCooked, nilValuesCooked.length))
        val in : InStreamFromByteChannel = start.inStream.asInstanceOf[InStreamFromByteChannel]

        val bytePos = (postEvalState.bitPos >> 3).toInt
        log(Debug("%s - Starting at bit pos: %s", eName, postEvalState.bitPos))
        log(Debug("%s - Starting at byte pos: %s", eName, bytePos))

        if (postEvalState.bitPos % 8 != 0) { return PE(start, "LiteralNilPattern - not byte aligned.") }

        log(Debug("Retrieving reader state."))
        val reader = getReader(bytePos, decoder.charset().name(), start)

        //        val byteReader = in.byteReader.atPos(bytePos)
        //        val reader = byteReader.charReader(decoder.charset().name())

        val d = new delimsearch.DelimParser()

        var result : delimsearch.DelimParseResult = new delimsearch.DelimParseResult

        result = d.parseInputPatterned(pattern, reader, decoder.charset())

        if (!result.isSuccess) {
          return PE(postEvalState, "%s - %s - Parse failed.", this.toString(), eName)
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
            return PE(postEvalState, "%s - Empty field found but not allowed!", eName)
          } else if (d.isFieldDfdlLiteral(field, nilValuesCooked.toSet)) {
            // Contains a nilValue, Success!
            val xsiNS = start.parentElement.getNamespace()
            start.parentElement.addContent(new org.jdom.Text(""))
            start.parentElement.setAttribute("nil", "true")

            val numBytes = result.field.getBytes(decoder.charset()).length
            //val endCharPos = start.charPos + result.field.length()
            val endCharPos = if (postEvalState.charPos == -1) result.field.length else postEvalState.charPos + result.field.length
            val endBitPos = (8 * numBytes) + start.bitPos

            log(Debug("%s - Found %s", eName, result.field))
            log(Debug("%s - Ended at byte position %s", eName, (endBitPos >> 3)))
            log(Debug("%s - Ended at bit position ", eName, endBitPos))

            //return postEvalState.withPos(endBitPos, endCharPos) // Need to advance past found nilValue
            return postEvalState.withReaderPos(endBitPos, endCharPos, reader) // Need to advance past found nilValue
          } else {
            // Fail!
            return PE(postEvalState, "%s - Does not contain a nil literal!", eName)
          }
        }
      }
    }

  }

  override def unparser : Unparser = new Unparser(e) {
    def unparse(start : UState) : UState = {
      Assert.notYetImplemented()
    }
  }
}

case class LiteralNilDelimitedOrEndOfData(e : ElementBase)
  extends StaticText(e.nilValue, e, "LiteralNilDelimitedOrEndOfData", e.isNillable) {
  lazy val unparserDelim = Assert.notYetImplemented()
  //  lazy val esObj = EscapeScheme.getEscapeScheme(es, e)
  val stParser = super.parser

  override def parser = new PrimParser(this, e) {
    override def toString = "LiteralNilDelimitedOrEndOfData(" + e.nilValue + ")"
    val decoder = e.knownEncodingDecoder

    def parse(start : PState) : PState = {
      withLoggingLevel(LogLevel.Info) {
        val eName = e.toString()

        // TODO: Why are we even doing this here?  LiteralNils don't care about delimiters
        // and LiteralNils don't have expressions! I think we can get rid of this variable
        // mapping stuff.
        //
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

        log(Debug("%s - Looking for: %s Count: %s", eName, delimsCooked, delimsCooked.length))
        val in : InStreamFromByteChannel = start.inStream.asInstanceOf[InStreamFromByteChannel]

        val bytePos = (postEvalState.bitPos >> 3).toInt
        log(Debug("%s - Starting at bit pos: %s", eName, postEvalState.bitPos))
        log(Debug("%s - Starting at byte pos: %s", eName, bytePos))

        if (postEvalState.bitPos % 8 != 0) { return PE(start, "LiteralNilDelimitedOrEndOfData - not byte aligned.") }

        log(Debug("Retrieving reader state."))
        val reader = getReader(bytePos, decoder.charset().name(), start)
        //        val byteReader = in.byteReader.atPos(bytePos)
        //        val reader = byteReader.charReader(decoder.charset().name())

        // 1. Parse up until terminating Markup
        // 2. Compare resultant field to nilValue(s)
        //		Same, success
        //		Diff, fail
        val d = new delimsearch.DelimParser()
        var result : delimsearch.DelimParseResult = new delimsearch.DelimParseResult

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
          return PE(postEvalState, "%s - %s - Parse failed.", this.toString(), eName)
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
            return PE(postEvalState, "%s - Empty field found but not allowed!", eName)
          } else if (d.isFieldDfdlLiteral(field, nilValuesCooked.toSet)) {
            // Contains a nilValue, Success!
            val xsiNS = start.parentElement.getNamespace()
            start.parentElement.addContent(new org.jdom.Text(""))
            start.parentElement.setAttribute("nil", "true")

            val numBytes = result.field.getBytes(decoder.charset()).length
            //val endCharPos = start.charPos + result.field.length()
            val endCharPos = if (postEvalState.charPos == -1) result.field.length else postEvalState.charPos + result.field.length
            val endBitPos = (8 * numBytes) + start.bitPos

            log(Debug("%s - Found %s", eName, result.field))
            log(Debug("%s - Ended at byte position %s", eName, (endBitPos >> 3)))
            log(Debug("%s - Ended at bit position ", eName, endBitPos))

            //return postEvalState.withPos(endBitPos, endCharPos) // Need to advance past found nilValue
            return postEvalState.withReaderPos(endBitPos, endCharPos, reader) // Need to advance past found nilValue
          } else {
            // Fail!
            return PE(postEvalState, "%s - Does not contain a nil literal!", eName)
          }
        }
      }
    }

  }

  override def unparser : Unparser = new Unparser(e) {
    def unparse(start : UState) : UState = {
      Assert.notYetImplemented()
    }
  }
}

case class LogicalNilValue(e : ElementBase) extends Primitive(e, e.isNillable)

// As soon as you turn these on (by removing the false and putting the real guard), then schemas all need to have
// these properties in them, which is inconvenient until we have multi-file schema support and format references.

case class LeadingSkipRegion(e : Term) extends Terminal(e, e.leadingSkip > 0) {
  e.schemaDefinition(e.leadingSkip < Compiler.maxSkipLength, "Property leadingSkip %s is larger than limit %s", e.leadingSkip, Compiler.maxSkipLength)

  def parser : Parser = new PrimParser(this, e) {
    def parse(pstate : PState) = {

      val newBitPos = 8 * (pstate.bytePos + e.leadingSkip)
      pstate.withPos(newBitPos, -1)
    }

    override def toString = "leadingSkip(" + e.leadingSkip + ")"
  }

  def unparser : Unparser = new Unparser(e) {
    def unparse(ustate : UState) = {

      Assert.notYetImplemented()
    }
  }
}

case class AlignmentFill(e : Term) extends Primitive(e, false) // e.alignment != AlignmentType.Implicit)

case class TrailingSkipRegion(e : Term) extends Primitive(e, false) // e.trailingSkip > 0)

case class PrefixLength(e : ElementBase) extends Primitive(e, e.lengthKind == LengthKind.Prefixed)

case class UnicodeByteOrderMark(e : GlobalElementDecl) extends Primitive(e, false)

case class FinalUnusedRegion(e : ElementBase) extends Primitive(e, false)

abstract class NewVariableInstanceBase(decl : AnnotatedSchemaComponent, stmt : DFDLNewVariableInstance)
  extends Terminal(decl, true) {
  val (uri, localName) = XMLUtils.QName(decl.xml, stmt.ref, decl.schemaDocument)
  val expName = XMLUtils.expandedQName(uri, localName)
}

case class NewVariableInstanceStart(decl : AnnotatedSchemaComponent, stmt : DFDLNewVariableInstance)
  extends NewVariableInstanceBase(decl, stmt) {

  def parser : Parser = new PrimParser(this, decl) {
    def parse(pstate : PState) = {
      Assert.notYetImplemented()
    }
  }

  def unparser : Unparser = Assert.notYetImplemented()

}

case class NewVariableInstanceEnd(decl : AnnotatedSchemaComponent, stmt : DFDLNewVariableInstance)
  extends NewVariableInstanceBase(decl, stmt) {

  def parser : Parser = new PrimParser(this, decl) {
    def parse(pstate : PState) = Assert.notYetImplemented()
  }

  def unparser : Unparser = Assert.notYetImplemented()
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
  assertKindName : String)
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
  assertKindName : String) extends AssertBase(decl, stmt.testTxt, stmt.message, discrim, assertKindName)

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
    "{ xs:boolean('true') }", // always true. We're just an assertion that says an initiator was found.
    "initiatedContent. This message should not be used.",
    true,
    "initiatedContent")

case class SetVariable(decl : AnnotatedSchemaComponent, stmt : DFDLSetVariable) extends Terminal(decl, true) {
  def parser : Parser = new SetVariableParser(decl, stmt)
  def unparser = DummyUnparser
}

case class InputValueCalc(e : ElementBase with ElementDeclMixin) extends Terminal(e, true) {

  def parser : Parser = new IVCParser(e)
  def unparser = DummyUnparser
}

abstract class ExpressionEvaluationParser(e : AnnotatedSchemaComponent)
  extends Parser(e) with WithParseErrorThrowing {

  override def toString = baseName + "(" + exprText + ")"

  def toBriefXML(depthLimit : Int = -1) = {
    "<" + baseName + ">" + exprText + "</" + baseName + ">"
  }

  def baseName : String
  def exprText : String
  def expandedTypeName : String

  lazy val expressionTypeSymbol = {
    // println(expandedTypeName)
    e.expressionCompiler.convertTypeString(expandedTypeName)
  }

  lazy val expr = e.expressionCompiler.compile(expressionTypeSymbol, exprText)

  // for unit testing
  def testExpressionEvaluation(elem : org.jdom.Element, vmap : VariableMap) = {
    val R(res, newVMap) = expr.evaluate(elem, vmap)
    R(res, newVMap)
  }

  def eval(start : PState) = {
    val currentElement = start.parentElement
    val R(res, newVMap) =
      expr.evaluate(currentElement, start.variableMap)
    // val result = res.toString // Everything in JDOM is a string!
    R(res, newVMap)
  }
}

class IVCParser(e : ElementBase with ElementDeclMixin)
  extends ExpressionEvaluationParser(e) {
  Assert.invariant(e.isSimpleType)
  val baseName = "InputValueCalc"

  lazy val Some(exprText) = e.inputValueCalcOption

  lazy val pt = e.primType
  lazy val ptn = pt.name
  lazy val expandedTypeName = XMLUtils.expandedQName(XMLUtils.XSD_NAMESPACE, ptn)

  def parse(start : PState) : PState =
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

class SetVariableParser(decl : AnnotatedSchemaComponent, stmt : DFDLSetVariable)
  extends ExpressionEvaluationParser(decl) {
  val baseName = "SetVariable[" + localName + "]"
  lazy val exprText = stmt.value
  val (uri, localName) = XMLUtils.QName(decl.xml, stmt.ref, decl.schemaDocument)
  val defv = decl.schema.schemaSet.getDefineVariable(uri, localName).getOrElse(
    stmt.schemaDefinitionError("Unknown variable: %s", stmt.ref))

  lazy val expandedTypeName = defv.extType

  def parse(start : PState) : PState =
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

case class BinaryExplicitLengthInBytes(e : ElementBase)
  extends Terminal(e, true)
  with WithParseErrorThrowing with BinaryReader {
  val expr = e.length
  val exprText = expr.prettyExpr

  def parser : Parser = new PrimParser(this, e) {
    override def toString = "BinaryExplicitLengthInBytes(" + exprText + ")"

    def parse(pstate : PState) : PState = withParseErrorThrowing(pstate) {
      log(Debug("Saving reader state."))
      setReader(pstate)

      log(Debug("Parsing starting at bit position: %s", pstate.bitPos))
      val R(nBytesAsAny, newVMap) = expr.evaluate(pstate.parent, pstate.variableMap)
      val nBytes = nBytesAsAny.asInstanceOf[Long]
      val start = pstate.withVariables(newVMap)
      if (nBytes > 8) {
        // Do Something Bad
        return PE(start, "Binary value exceeds the limit allowed by the processing subsystem: %s", nBytes)
      }

      log(Debug("Explicit length %s", nBytes))

      // TODO: Longest possible field is Long

      val in = start.inStream

      // TODO: Get requested bytes from stream

      if (false) {
        // Do Something Bad
        return PE(start, "Insufficent Bits in field; required %s", nBytes * 8)
      }

      // log(Debug("Parsed: " + result))
      // log(Debug("Ended at bit position " + endBitPos))
      // val endCharPos = start.charPos + result.length
      val currentElement = start.parentForAddContent
      // Note: this side effect is backtracked, because at points of uncertainty, pre-copies of a node are made
      // and when backtracking occurs they are used to replace the nodes modified by sub-parsers.
      // currentElement.addContent(new org.jdom.Text(result))
      // val postState = start.withPos(endBitPos, endCharPos)
      // postState
      pstate
    }
  }

  def unparser : Unparser = new Unparser(e) {
    override def toString = "BinaryExplicitLengthInBytesUnparser(" + exprText + ")"

    def unparse(start : UState) : UState = {
      Assert.notYetImplemented()
    }
  }
}

case class BinaryExplicitLengthInBits(e : ElementBase)
  extends Terminal(e, true)
  with WithParseErrorThrowing {
  val expr = e.length
  val exprText = expr.prettyExpr

  def parser : Parser = new PrimParser(this, e) {
    override def toString = "BinaryExplicitLengthInBitsParser(" + exprText + ")"

    def parse(start : PState) : PState = {
      Assert.notYetImplemented()
    }
  }
  def unparser : Unparser = new Unparser(e) {
    override def toString = "BinaryExplicitLengthInBitsUnparser(" + exprText + ")"

    def unparse(start : UState) : UState = {
      Assert.notYetImplemented()
    }
  }

}

case class StringExplicitLengthInBytes(e : ElementBase)
  extends Terminal(e, true)
  with WithParseErrorThrowing with BinaryReader {
  val expr = e.length
  val exprText = expr.prettyExpr
  val decoder = e.knownEncodingDecoder
  // val maxBytes = daffodil.compiler.Compiler.maxFieldContentLengthInBytes
  var cbuf : CharBuffer = CharBuffer.allocate(0) // TODO: Performance: get a char buffer from a pool.
  var cbufSize = 0

  def parser : Parser = new PrimParser(this, e) {
    override def toString = "StringExplicitLengthInBytesParser(" + exprText + ")"

    def parse(pstate : PState) : PState = withParseErrorThrowing(pstate) {
      log(Debug("Saving reader state."))
      setReader(pstate)

      log(Debug("Parsing starting at bit position: %s", pstate.bitPos))

      val R(nBytesAsAny, newVMap) = expr.evaluate(pstate.parent, pstate.variableMap)
      val nBytes = nBytesAsAny.asInstanceOf[Long]
      val start = pstate.withVariables(newVMap)
      log(Debug("Explicit length %s", nBytes))

      if (start.bitPos % 8 != 0) { return PE(start, "StringExplicitLengthInBytes - not byte aligned.") }

      val in : InStreamFromByteChannel = start.inStream.asInstanceOf[InStreamFromByteChannel]

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
        case e : java.nio.BufferUnderflowException => { return PE(start, "StringExplicitLengthInBytesParser - Insufficient Bits in field; required %s", nBytes * 8) }
        case e : IndexOutOfBoundsException => { return PE(start, "StringExplicitLengthInBytesParser - IndexOutOfBounds: \n%s", e.getMessage()) }
        case u : UnsuppressableException => throw u
        case e : Exception => { return PE(start, "StringExplicitLengthInBytesParser - Exception: \n%s", e.getStackTraceString) }
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

  def unparser : Unparser = new Unparser(e) {
    override def toString = "StringExplicitLengthInBytesUnparser(" + exprText + ")"
    //    val encoder = e.knownEncodingEncoder

    def unparse(start : UState) : UState = {
      Assert.notYetImplemented()
      //      // setLoggingLevel(LogLevel.Info)
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

trait TextReader extends Logging {

  def getReader(bytePos : Int, csName : String, state : PState) : DFDLCharReader = {
    withLoggingLevel(LogLevel.Info) {
      // Do we already have a reader in the PState?
      val res = state.textReader match {
        case Some(rdr) if (rdr.getCharsetName == csName && state.charPos != -1) => rdr
        case None => {
          if (state.charPos == -1) {
            val in : InStreamFromByteChannel = state.inStream.asInstanceOf[InStreamFromByteChannel]
            in.byteReader.atPos(bytePos).newCharReader(csName).asInstanceOf[DFDLCharReader]
          } else {
            // Retrieve one if possible
            // TODO: Does the underlying call throw if we are out of data? How will this fail?
            val in : InStreamFromByteChannel = state.inStream.asInstanceOf[InStreamFromByteChannel]
            //System.err.println(in.byteReader.bb.array().toList.map(b => b.toByte.toHexString))
            in.byteReader.atPos(bytePos).charReader(csName).asInstanceOf[DFDLCharReader]
          }

        }
      }
      log(Debug("Retrieval complete."))
      res
    }
  }

  // For optimizing performance, is it necessary for every text reader to update
  // the hash with the updated charPos?  Probably not.

}

trait BinaryReader extends Logging {

  def setReader(state : PState) = {
    withLoggingLevel(LogLevel.Info) {
      // If a reader exists, save it
      //
      // We assume that the reader is already pointed to the
      // appropriate charPos as it should've been set
      // via state.withPos
      state.textReader match {
        case Some(rdr) => {
          val in : InStreamFromByteChannel = state.inStream.asInstanceOf[InStreamFromByteChannel]
          in.byteReader.updateCharReader(rdr)
        }
        case None => {
          // Nothing to do
        }
      }
      log(Debug("Save complete."))
    }
  }

}

