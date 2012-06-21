package daffodil.grammar

import org.jdom._
import daffodil.xml._
import daffodil.xml._
import daffodil.processors._
import daffodil.exceptions.Assert
import daffodil.schema.annotation.props._
import daffodil.dsom._
import daffodil.api._
import java.nio._
import java.nio.charset._
import stringsearch._
import scala.collection.JavaConversions._
import scala.util.logging.ConsoleLogger
import stringsearch.DelimSearcherV3._
import stringsearch.DelimSearcherV3.SearchResult
import scala.collection.mutable.Queue

/**
 * Encapsulates lower-level parsing with a uniform interface
 */
trait Parser {

  def parse(pstate: PState): PState

  // TODO: other methods for things like asking for the ending position of something
  // which would enable fixed-length formats to skip over data and not parse it at all.

}

class EmptyGramParser extends Parser {
  def parse(pstate: PState) = pstate
}

class SeqCompParser(p: Gram, q: Gram) extends Parser {
  Assert.invariant(!p.isEmpty && !q.isEmpty)
  val pParser = p.parser
  val qParser = q.parser
  def parse(pstate: PState) = {
    val pResult = pParser.parse(pstate)
    if (pResult.status == Success) {
      val qResult = qParser.parse(pResult)
      qResult
    } else pResult
  }

  override def toString = pParser.toString + " ~ " + qParser.toString
}

class AltCompParser(p: Gram, q: Gram) extends Parser {
  Assert.invariant(!p.isEmpty && !q.isEmpty)
  val pParser = p.parser
  val qParser = q.parser
  def parse(pstate: PState) = {

    // TODO: capture current Infoset node (make a shallow copy of it)
    // restoring this later (literally, clobbering the parent to point to 
    // this copy), is the way we backtrack the side-effects on the Infoset.
    //
    // TBD: better Scala idiom for try/catch than this var stuff
    // Use of var makes this code non-thread-safe. If you can avoid var, then you
    // don't have to worry about multiple threads at all.
    //
    var pResult: PState = null
    try {
      pResult = pParser.parse(pstate)
    } catch {
      case e: Exception =>
    }
    if (pResult != null && pResult.status == Success) pResult
    else {
      // TODO: Unwind any side effects on the Infoset 
      //
      // TODO: check for discriminator evaluated to true.
      // If so, then we don't run the next alternative, we
      // consume this discriminator status result (so it doesn't ripple upward)
      // and return the failed state.
      //
      val qResult = qParser.parse(pstate)
      qResult
    }
  }

  override def toString = "(" + pParser.toString + " | " + qParser.toString + ")"
}

class RepExactlyNParser(n: Long, r: => Gram) extends Parser {
  Assert.invariant(!r.isEmpty)
  val rParser = r.parser
  def parse(pstate: PState): PState = {
    val intN = n.toInt // TODO: Ints aren't big enough for this.
    var pResult = pstate
    1 to intN foreach { _ =>
      {
        val pNext = rParser.parse(pResult)
        if (pNext.status != Success) return pNext
        pResult = pNext
      }
    }
    pResult
  }

  override def toString = "RepExactlyNParser(" + rParser.toString + ")"
}

class RepUnboundedParser(r: => Gram) extends Parser {
  Assert.invariant(!r.isEmpty)
  val rParser = r.parser
  def parse(pstate: PState): PState = {

    var pResult = pstate
    while (pResult.status == Success) {

      val cloneNode = pResult.captureJDOM
      val pNext = rParser.parse(pResult)
      if (pNext.status != Success) {
        pResult.restoreJDOM(cloneNode)
        System.err.println("Failure suppressed.")
        return pResult
      }
      pResult = pNext

    }
    Assert.invariantFailed("Unbounded loop terminated wrong")
  }

  override def toString = "RepUnboundedParser(" + rParser.toString + ")"
}

case class DummyParser(sc: PropertyMixin) extends Parser {
  def parse(pstate: PState): PState = Assert.abort("Parser for " + sc + " is not yet implemented.")
  override def toString = if (sc == null) "Dummy[null]" else "Dummy[" + sc.detailName + "]"
}

class GeneralParseFailure(msg : String) extends Diagnostic {
  def isError() = true
  def getSchemaLocations() = Nil
  def getDataLocations() = Nil
  def getMessage() = msg
}

class GeneralUnparseFailure(msg : String) extends Diagnostic {
  def isError() = true
  def getSchemaLocations() = Nil
  def getDataLocations() = Nil
  def getMessage() = msg
}

/**
 * A parser takes a state, and returns an updated state
 *
 * The fact that there are side-effects/mutations on parts of the state
 * enables us to reuse low-level java primitives that mutate streams.
 *
 * The goal however, is to hide that fact so that the only places that have to
 * know are the places doing the mutation, and the places rolling them back
 * which should be isolated to the alternative parser.
 */
class PState(
  val inStream: InStream,
  val bitPos: Long,
  val bitLimit: Long,
  val charPos: Long,
  val charLimit: Long,
  val parent: org.jdom.Element,
  val variableMap: VariableMap,
  val target: String,
  val namespaces: Namespaces,
  val status: ProcessorResult,
  val groupIndexStack: List[Long],
  val childIndexStack: List[Long],
  val arrayIndexStack: List[Long],
  val diagnostics : List[Diagnostic]) {
  def bytePos = bitPos >> 3
  def whichBit = bitPos % 8
  def groupPos = groupIndexStack.head
  def childPos = childIndexStack.head


  /**
   * Convenience functions for creating a new state, changing only
   * one or a related subset of the state components to a new one.
   */
  def withInStream(inStream: InStream, status: ProcessorResult = Success) =
    new PState(inStream, bitPos, bitLimit, charPos, charLimit, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, diagnostics)
  def withPos(bitPos: Long, charPos: Long, status: ProcessorResult = Success) =
    new PState(inStream, bitPos, bitLimit, charPos, charLimit, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, diagnostics)
  def withParent(parent: org.jdom.Element, status: ProcessorResult = Success) =
    new PState(inStream, bitPos, bitLimit, charPos, charLimit, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, diagnostics)
  def withVariables(variableMap: VariableMap, status: ProcessorResult = Success) =
    new PState(inStream, bitPos, bitLimit, charPos, charLimit, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, diagnostics)
  def withGroupIndexStack(groupIndexStack: List[Long], status: ProcessorResult = Success) =
    new PState(inStream, bitPos, bitLimit, charPos, charLimit, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, diagnostics)
  def withChildIndexStack(childIndexStack: List[Long], status: ProcessorResult = Success) =
    new PState(inStream, bitPos, bitLimit, charPos, charLimit, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, diagnostics)
  def withArrayIndexStack(arrayIndexStack: List[Long], status: ProcessorResult = Success) =
    new PState(inStream, bitPos, bitLimit, charPos, charLimit, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, diagnostics)
  def failed(msg: => String) : PState =
    failed(new GeneralParseFailure(msg))
  def failed(failureDiagnostic: Diagnostic) =
    new PState(inStream, bitPos, bitLimit, charPos, charLimit, parent, variableMap, target, namespaces, new Failure(failureDiagnostic.getMessage()), groupIndexStack, childIndexStack, arrayIndexStack, failureDiagnostic :: diagnostics)

  /**
   * advance our position, as a child element of a parent, and our index within the current sequence group.
   *
   * These can be different because an element can have sequences nested directly in sequences. Those effectively all
   * get flattened into children of the element. The start of a sequence doesn't start the numbering of children. It's
   * the start of a complex type that does that.
   */
  def moveOverByOne = {
    val s1 = groupIndexStack match {
      case Nil => this
      case hd :: tl => {
        val newGroupIndex = hd + 1
        this.withGroupIndexStack(newGroupIndex :: tl)
      }
    }
    val s2 = s1.childIndexStack match {
      case Nil => s1
      case hd :: tl => {
        val newChildIndex = hd + 1
        s1.withChildIndexStack(newChildIndex :: tl)
      }
    }
    val s3 = s2.arrayIndexStack match {
      case Nil => s2
      case hd :: tl => {
        val newArrayIndex = hd + 1
        s1.withArrayIndexStack(newArrayIndex :: tl)
      }
    }
    s3
  }

  def captureJDOM: Int = {
    parent.getContentSize()
  }

  def restoreJDOM(previousContentSize: Int) = {
    for (i <- previousContentSize until parent.getContentSize()) {
      parent.removeContent(i)
    }
    this
    //    val pp = parent.getParent().asInstanceOf[org.jdom.Element] // Parent's Parent.
    //    val pi :: ppi :: rest = childIndexStack
    //    pp.setContent(ppi.toInt, newElem)
    //    newElem
  }
}

object PState {

  /**
   * Initialize the state block given our InStream and a root element declaration.
   */
  def createInitialState(rootElemDecl: GlobalElementDecl, in: InStream): PState = {
    val inStream = in
    val doc = new org.jdom.Element("_document_") // a dummy element to be parent of the root. We don't use the org.jdom.Document type.
    val variables = new VariableMap()
    val targetNamespace = rootElemDecl.schemaDocument.targetNamespace
    val namespaces = new Namespaces()
    val status = Success
    val groupIndexStack = Nil
    val childIndexStack = Nil
    val arrayIndexStack = Nil
    val diagnostics = Nil
    val newState = new PState(inStream, 0, -1, 0, -1, doc, variables, targetNamespace, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, diagnostics)
    newState
  }

  /**
   * For testing it is convenient to just hand it strings for data.
   */
  def createInitialState(rootElemDecl: GlobalElementDecl, data: String): PState = {
    val in = Compiler.stringToReadableByteChannel(data)
    createInitialState(rootElemDecl, in, data.length)
  }

  /**
   * Construct our InStream object and initialize the state block.
   */
  def createInitialState(rootElemDecl: GlobalElementDecl, input: DFDL.Input, sizeHint: Long = -1): PState = {
    val inStream =
      if (sizeHint != -1) new InStreamFromByteChannel(input, sizeHint)
      else new InStreamFromByteChannel(input)
    createInitialState(rootElemDecl, inStream)
  }
}

/**
 * Encapsulates the I/O as an abstraction that works something like a java.nio.ByteBuffer
 * but a bit more specialized for DFDL needs, e.g., supports offsets and positions in bits.
 */

trait InStream {
  /**
   * These return a value of the appropriate type, or they throw
   * an exception when there is no more data, or if the offset is past the end of data,
   * or if the offset exceeds implementation capacity such as for moving backwards in
   * the data beyond buffering capacity.
   */
  //  def getBinaryLong(bitOffset : Long,  isBigEndian : Boolean) : Long
  //  def getBinaryInt(bitOffset : Long,  isBigEndian : Boolean) : Int

  def fillCharBuffer(buf: CharBuffer, bitOffset: Long, decoder: CharsetDecoder): Long

  def getInt(bitPos: Long, order: java.nio.ByteOrder): Int
  def getDouble(bitPos: Long, order: java.nio.ByteOrder): Double
  def getFloat(bitPos: Long, order: java.nio.ByteOrder): Float

  // def fillCharBufferUntilDelimiterOrEnd
}

class InStreamFromByteChannel(in: DFDL.Input, sizeHint: Long = 1024 * 128) extends InStream { // 128K characters by default.
  val maxCharacterWidthInBytes = 4 // worst case. Ok for testing. Don't use this pessimistic technique for real data.
  var bb = ByteBuffer.allocate(maxCharacterWidthInBytes * sizeHint.toInt) // FIXME: all these Int length limits are too small for large data blobs
  // Verify there is not more data by making sure the buffer was not read to capacity.
  var count = in.read(bb) // just pull it all into the byte buffer
  if (count == bb.capacity) {
    // Buffer not big enough, allocate one 4 times larger and fill at offset
    var tooSmall = scala.collection.mutable.ListBuffer.empty[ByteBuffer]
    var lastWrite = 0
    while (count == bb.capacity()) {
      // Remember where we started
      bb.flip()
      bb.position(lastWrite)

      // Save old buffer and allocate anew
      tooSmall += bb
      bb = ByteBuffer.allocate(count * 4)

      // Leave space to copy the old buffers back to this one
      bb.position(count)
      lastWrite = count

      // Read in as much as possible
      count += in.read(bb)
    }
    // bb now holds enough space for the entire buffer starting from a position at the end of the previous buffer's size
    // so copy over the other buffers in tooSmall to fill in the gap
    bb.flip()
    tooSmall.foreach(b => { bb.put(b) } )
    bb.position(0)
  }
  else {
    // Buffer is sufficiently sized
    bb.flip()
  }

  // System.err.println("InStream byte count is " + count)
  // note, our input data might just be empty string, in which case count is zero, and that's all legal.
  def fillCharBuffer(cb: CharBuffer, bitOffset: Long, decoder: CharsetDecoder): Long = {
    Assert.subset(bitOffset % 8 == 0, "characters must begin on byte boundaries")
    val byteOffsetAsLong = (bitOffset >> 3)
    Assert.subset(byteOffsetAsLong <= Int.MaxValue, "maximum offset (in bytes) cannot exceed Int.MaxValue")
    val byteOffset = byteOffsetAsLong.toInt
    // 
    // Note: not thread safe. We're depending here on the byte buffer being private to us.
    //
    //    System.err.println("Decode ByteBufferAsCharBuffer: " + bb.asCharBuffer().toString())
    bb.position(byteOffset)
    //    System.err.println("Decode ByteOffset: " + byteOffset)
    //    System.err.println("Decode ByteBuffer: " + bb.toString())
    //    System.err.println("Decode CharFromByteBuffer: " + bb.getChar(byteOffset))
    //    System.err.println("Decode ByteBufferAsCharBuffer: " + bb.asCharBuffer().toString())
    //    while (bb.hasRemaining()){
    //      System.err.print(bb.get().toHexString + " ")
    //    }
    //    System.err.println
    decoder.reset()
    val cr1 = decoder.decode(bb, cb, true) // true means this is all the input you get.
    System.err.println("Decode Error1: " + cr1.toString())
    if (cr1 != CoderResult.UNDERFLOW) {
      if (cr1 == CoderResult.OVERFLOW) {
        // it's ok. It just means we've satisfied the char buffer.
      } else // for some parsing, we need to know we couldn't decode, but this is expected behavior.
        return -1L // Assert.abort("Something went wrong while decoding characters: CoderResult = " + cr1)   
    }
    val cr2 = decoder.flush(cb)
    System.err.println("Decode Error2: " + cr2.toString())
    if (cr2 != CoderResult.UNDERFLOW) {
      // Something went wrong
      return -1L // Assert.abort("Something went wrong while decoding characters: CoderResult = " + cr2) 
      // FIXME: proper handling of errors. Some of which are 
      // to be suppressed, other converted, others skipped, etc. 
    }
    cb.flip() // so the caller can now read the sb.

    val endBytePos = bb.position()

    bb.position(0) // prevent anyone depending on the buffer position across calls to any of the InStream methods.
    val endBitPos: Long = endBytePos << 3

    endBitPos
  }

  // System.err.println("InStream byte count is " + count)
  // note, our input data might just be empty string, in which case count is zero, and that's all legal.
  def fillStringBuilder(sb: StringBuilder, bitOffset: Long, decoder: CharsetDecoder): Long = {
    Assert.subset(bitOffset % 8 == 0, "characters must begin on byte boundaries")
    val byteOffsetAsLong = (bitOffset >> 3)
    Assert.subset(byteOffsetAsLong <= Int.MaxValue, "maximum offset (in bytes) cannot exceed Int.MaxValue")
    val byteOffset = byteOffsetAsLong.toInt
    //
    // Note: not thread safe. We're depending here on the byte buffer being private to us.
    //
    //    System.err.println("Decode ByteBufferAsCharBuffer: " + bb.asCharBuffer().toString())
    bb.position(byteOffset)
    //    System.err.println("Decode ByteOffset: " + byteOffset)
    //    System.err.println("Decode ByteBuffer: " + bb.toString())
    //    System.err.println("Decode CharFromByteBuffer: " + bb.getChar(byteOffset))
    //    System.err.println("Decode ByteBufferAsCharBuffer: " + bb.asCharBuffer().toString())
    //    while (bb.hasRemaining()){
    //      System.err.print(bb.get().toHexString + " ")
    //    }
    //    System.err.println
    decoder.reset()
    var cb = CharBuffer.allocate(sb.capacity)
    // TODO: How do I get an immutable CharBuffer from a StringBuilder?? sb.copyToArray(cb)
    val cr1 = decoder.decode(bb, cb, true) // true means this is all the input you get.
    System.err.println("Decode Error1: " + cr1.toString())
    if (cr1 != CoderResult.UNDERFLOW) {
      if (cr1 == CoderResult.OVERFLOW) {
        // it's ok. It just means we've satisfied the char buffer.
      } else // for some parsing, we need to know we couldn't decode, but this is expected behavior.
        return -1L // Assert.abort("Something went wrong while decoding characters: CoderResult = " + cr1)
    }
    // TODO: How do I flush the decoder with a StringBuilder?? val cr2 = decoder.flush(sb)
    // TODO: How do I calculate cr2??  System.err.println("Decode Error2: " + cr2.toString())
    // TODO: How do I calculate cr2??  if (cr2 != CoderResult.UNDERFLOW) {
      // Something went wrong
      return -1L // Assert.abort("Something went wrong while decoding characters: CoderResult = " + cr2)
      // FIXME: proper handling of errors. Some of which are
      // to be suppressed, other converted, others skipped, etc.
    // TODO: How do I check if there was an Underflow??  }
    // TODO: What does flip do and how do you do it on a StringBuilder? sb.flip() // so the caller can now read the sb.

    val endBytePos = bb.position()

    bb.position(0) // prevent anyone depending on the buffer position across calls to any of the InStream methods.
    val endBitPos: Long = endBytePos << 3

    endBitPos
  }

  // System.err.println("InStream byte count is " + count)
  // note, our input data might just be empty string, in which case count is zero, and that's all legal.
  def fillCharBuffer2(cb: CharBuffer, bitOffset: Long, decoder: CharsetDecoder): (Long, Boolean) = {
    Assert.subset(bitOffset % 8 == 0, "characters must begin on byte boundaries")
    val byteOffsetAsLong = (bitOffset >> 3)
    Assert.subset(byteOffsetAsLong <= Int.MaxValue, "maximum offset (in bytes) cannot exceed Int.MaxValue")
    val byteOffset = byteOffsetAsLong.toInt
    // 
    // Note: not thread safe. We're depending here on the byte buffer being private to us.
    //
    bb.position(byteOffset)
    decoder.reset()
    val cr1 = decoder.decode(bb, cb, true) // true means this is all the input you get.
    if (cr1 != CoderResult.UNDERFLOW) {
      if (cr1 == CoderResult.OVERFLOW) {
        // it's ok. It just means we've satisfied the char buffer.
      } else // for some parsing, we need to know we couldn't decode, but this is expected behavior.
        return (-1L, true) // Assert.abort("Something went wrong while decoding characters: CoderResult = " + cr1)   
    }
    val cr2 = decoder.flush(cb)
    if (cr2 != CoderResult.UNDERFLOW) {
      // Something went wrong
      return (-1L, true) // Assert.abort("Something went wrong while decoding characters: CoderResult = " + cr2) 
      // FIXME: proper handling of errors. Some of which are 
      // to be suppressed, other converted, others skipped, etc. 
    }
    cb.flip() // so the caller can now read the sb.

    val endBytePos = bb.position()

    var EOF: Boolean = bb.limit() == bb.position()

    bb.position(0) // prevent anyone depending on the buffer position across calls to any of the InStream methods.
    val endBitPos: Long = endBytePos << 3

    (endBitPos, EOF)
  }

  import SearchResult._
  import stringsearch.delimiter._
  def fillCharBufferUntilDelimiterOrEnd(cb: CharBuffer, bitOffset: Long, decoder: CharsetDecoder, delimiters: Set[String]): (String, Long, SearchResult, Delimiter) = {
    println("===\nSTART_FILL!\n===\n")
    val byteOffsetAsLong = (bitOffset >> 3)
    val byteOffset = byteOffsetAsLong.toInt

    var (endBitPosA: Long, state) = fillCharBufferMixedData(cb, bitOffset, decoder)
    var sb: StringBuilder = new StringBuilder // To keep track of the searched text
    val dSearch = new DelimSearcher with ConsoleLogger
    var buf = cb
    
    if (endBitPosA == -1L){
      System.err.println("Failed, reached end of buffer.")
      return (cb.toString(), -1L, SearchResult.NoMatch, null)
    }
    
    println("START_CB: " + cb.toString())

    delimiters foreach {
      x => dSearch.addDelimiter(x)
    }

    var (theState, result, endPos, endPosDelim, theDelimiter) = dSearch.search(buf, 0)

    if (theState == SearchResult.FullMatch) {
      sb.append(result)
    }
    var EOF: Boolean = false // Did we run off the end of the buffer?
    
    if (buf.toString().length == 0){ EOF = true } // Buffer was empty to start, nothing to do.

    // Proceed until we encounter a FullMatch or EOF
    while ((theState == SearchResult.NoMatch || theState == SearchResult.PartialMatch) && endBitPosA != -1 && !EOF) {
      buf.clear()
      buf = CharBuffer.allocate(buf.length() * 2)

      val fillState = fillCharBufferMixedData(buf, bitOffset, decoder)
      endBitPosA = fillState._1
      EOF = fillState._2

      var (state2, result2, endPos2, endPosDelim2, theDelimiter2) = dSearch.search(buf, 0, true)
      theState = state2
      endPos = endPos2
      theDelimiter = theDelimiter2

      if (theState != SearchResult.PartialMatch) {
        sb.append(result2)
      }
    }

    // Encode the found string in order to calculate
    // the ending position of the ByteBuffer
    //
    val charSet = decoder.charset()
    val resBB = charSet.encode(sb.toString())

    // Calculate the new ending position of the ByteBuffer
    if (endPos != -1) {
      endBitPosA = (endPos << 3) + bitOffset
    } else {
      endBitPosA = (resBB.limit() << 3) + bitOffset
    }

    println("FILL - CB: " + sb.toString() + ", EndBitPos: " + endBitPosA)
    println("===\nEND_FILL!\n===\n")
    (sb.toString(), endBitPosA, theState, theDelimiter)
  }

  def fillCharBufferWithPatternMatch(sb: StringBuilder, bitOffset: Long, decoder: CharsetDecoder, delimiters: Set[String]): (String, Long, SearchResult) = {
    println("===\nSTART_FILL!\n===\n")
    val byteOffsetAsLong = (bitOffset >> 3)

    val byteOffset = byteOffsetAsLong.toInt

    println("BYTE_OFFSET: " + byteOffset)
    var endBitPosA: Long = fillStringBuilder(sb, bitOffset, decoder)
    val dSearch = new DelimSearcher with ConsoleLogger
    var buf = sb

    delimiters foreach {
      x => dSearch.addDelimiter(x)
    }

    // --
    dSearch.printDelimStruct
    // --

    println("CB_" + sb.toString() + "_END_CB")

    //var (theState, result, endPos) = dSearch.search(buf, 0)
    var imBuffer = CharBuffer.allocate(buf.capacity)
    // TODO: How do I get an immutable CharBuffer from a StringBuilder?? sb.copyToArray(imBuffer)
    var (theState, result, endPos, endPosDelim, theDelimiter) = dSearch.search(imBuffer, 0)

    if (theState == SearchResult.FullMatch) {
      sb.append(result)
    }
    var EOF: Boolean = false

    if (buf.toString().length == 0){ EOF = true } // Buffer was empty to start, nothing to do.

    // Proceed until we encounter a FullMatch or EOF
    while ((theState == SearchResult.NoMatch || theState == SearchResult.PartialMatch) && endBitPosA != -1 && !EOF) {
      buf.clear()
      // TODO: Since buf is a StringBuilder, so I need to increase the size??  buf = CharBuffer.allocate(buf.length * 2)

      // TODO: What does this do??  val fillState = fillCharBuffer2(buf, bitOffset, decoder)
      // TODO: How do I calculate endBitPosA??  endBitPosA = fillState._1
      // TODO: How do I calculate EOF??  EOF = fillState._2

      //var (state2, result2, endPos2) = dSearch.search(buf, endPos, false)
      // var (state2, result2, endPos2, endPosDelim2) = dSearch.search(buf, endPos, false)
      // TODO: Convert to Regex??  var (state2, result2, endPos2, endPosDelim2) = dSearch.search(buf, 0, true)
      // TODO: How do I calculate state2??  theState = state2
      // TODO: How do I calculate endPos??  endPos = endPos2

      if (theState != SearchResult.PartialMatch) {
        // TODO: How do I calculate result2??  sb.append(result2)
      }
    }

    // Encode the found string in order to calculate
    // the ending position of the ByteBuffer
    //
    val charSet = decoder.charset()
    val resBB = charSet.encode(sb.toString())

    println("ENDPOS_FillCharBuffer: " + endPos)

    // Calculate the new ending position of the ByteBuffer
    if (endPos != -1) {
      endBitPosA = (endPos << 3) + bitOffset
    } else {
      endBitPosA = (resBB.limit() << 3) + bitOffset
    }

    println("FILL - CB: " + sb.toString() + ", EndBitPos: " + endBitPosA)
    println("===\nEND_FILL!\n===\n")
    (sb.toString(), endBitPosA, theState)
  }
  
  def decodeNBytes(N: Int, array: Array[Byte], decoder: CharsetDecoder): CharBuffer = {
    val list: Queue[Byte] = Queue.empty
    for (i <- 0 to N - 1){
      list += array(i).toByte
    }
    val cb = decoder.decode(ByteBuffer.wrap(list.toArray[Byte]))
    cb
  }
  
  def decodeUntilFail(bytesArray: Array[Byte], decoder: CharsetDecoder, endByte: Int): (CharBuffer, Int) = {
    var cbFinal: CharBuffer = CharBuffer.allocate(1)
    var cbPrev: CharBuffer = CharBuffer.allocate(1)
    var numBytes: Int = 1
    try {
      while(numBytes <= endByte){
        cbPrev = decodeNBytes(numBytes, bytesArray, decoder)
        cbFinal = cbPrev
        numBytes += 1
      }
    } catch {
      case e: Exception => System.err.println("Exception in decodeUntilFail: " + e.toString())
    }
    (cbFinal, (numBytes-1))
  }
  
  // Fills the CharBuffer with as many bytes as can be decoded successfully.
  //
  def fillCharBufferMixedData(cb: CharBuffer, bitOffset: Long, decoder: CharsetDecoder): (Long, Boolean) = {
    Assert.subset(bitOffset % 8 == 0, "characters must begin on byte boundaries")
    val byteOffsetAsLong = (bitOffset >> 3)
    Assert.subset(byteOffsetAsLong <= Int.MaxValue, "maximum offset (in bytes) cannot exceed Int.MaxValue")
    val byteOffset = byteOffsetAsLong.toInt
    // 
    // Note: not thread safe. We're depending here on the byte buffer being private to us.
    //
    System.err.println("Start at byteOffset " + byteOffset)
    System.err.println("byteBuffer limit: " + bb.limit())
    
    if (byteOffset >= bb.limit()){
      // We are at end, nothing more to do.
      System.err.println("byteOffset >= limit! Nothing more to do.")
      return (-1L, true)
    }
    
    bb.position(byteOffset) // Set byte position of ByteBuffer to the offset
    decoder.reset()
    
    var byteArray: Array[Byte] = new Array[Byte](bb.limit() - byteOffset)
    
    // Retrieve a byte array from offset to end of ByteBuffer.
    // Starts at 0, because ByteBuffer was already set to byteOffset
    // Ends at ByteBuffer limit in Bytes minus the offset
    bb.get(byteArray, 0, (bb.limit - byteOffset))
    
    var (result:CharBuffer, bytesDecoded: Int) = decodeUntilFail(byteArray, decoder, bb.limit())
    
    if (bytesDecoded == 0){ return (-1L, true) }
    
    println("MixedDataResult: " + result + " bytesDecoded: " + bytesDecoded)
    
    cb.clear()
    cb.append(result)
    
    cb.flip() // so the caller can now read the sb.

    val endBytePos = byteOffset + bytesDecoded
    
    System.err.println("Ended at byte pos " + endBytePos)
    
    var EOF: Boolean = bb.limit() == bb.position()

    bb.position(0) // prevent anyone depending on the buffer position across calls to any of the InStream methods.
    val endBitPos: Long = endBytePos << 3
    
    System.err.println("Ended at bit pos " + endBitPos)
    
    (endBitPos, EOF)
  }
  
  // Read the delimiter if possible off of the ByteBuffer
  //
  def getDelimiter(cb: CharBuffer, bitOffset: Long, decoder: CharsetDecoder, delimiters: Set[String]): (String, Long, Long, SearchResult, Delimiter) = {
    println("===\nSTART_GET_DELIMITER!\n===\n")
   
    val byteOffsetAsLong = (bitOffset >> 3)

    val byteOffset = byteOffsetAsLong.toInt

    println("BYTE_OFFSET: " + byteOffset)
    
    var (endBitPos: Long, state) = fillCharBufferMixedData(cb, bitOffset, decoder)
    var endBitPosA: Long = endBitPos
    
    if (endBitPos == -1L){
      System.err.println("Failed, reached end of buffer.")
      return (cb.toString(), -1L, -1L, SearchResult.NoMatch, null)
    }
    
    var sb: StringBuilder = new StringBuilder // To keep track of the searched text
    val dSearch = new DelimSearcher
    var buf = cb

    delimiters foreach { x => dSearch.addDelimiter(x) }

    println("CB: " + cb.toString())

    var (theState, result, endPos, endPosDelim, theDelimiter) = dSearch.search(buf, 0)
    
    if (theState == SearchResult.FullMatch) {
      sb.append(result)
    }
    
    var EOF: Boolean = false  // Flag to indicate if we ran out of data to fill CharBuffer with
    
    if (buf.toString().length == 0){ EOF = true } // Buffer was empty to start, nothing to do

    // Proceed until we encounter a FullMatch or EOF (we ran out of data)
    while ((theState == SearchResult.NoMatch || theState == SearchResult.PartialMatch) && endBitPosA != -1 && !EOF) {
      buf.clear()
      buf = CharBuffer.allocate(buf.length() * 2)

      val fillState = fillCharBufferMixedData(buf, bitOffset, decoder)
      endBitPosA = fillState._1
      EOF = fillState._2 // Determine if we ran out of data to fill the CharBuffer with

      var (state2, result2, endPos2, endPosDelim2, theDelimiter2) = dSearch.search(buf, endPosDelim, false)
      println("GET_DELIMITER_LOOP: " + state2 + " " + result2 + " " + endPos2 + " " + endPosDelim2)
      theState = state2 // Determine if there was a Full, Partial or No Match
      endPos = endPos2  // Start of delimiter
      endPosDelim = endPosDelim2 // End of delimiter
      theDelimiter = theDelimiter2

      if (theState != SearchResult.PartialMatch) {
        sb.append(result2)
      }
    }
    
    var delimLength = endPosDelim - endPos
    
    if (endPosDelim == 0 && endPos == 0 && theState == SearchResult.FullMatch){ delimLength = 1}

    // Encode the found string in order to calculate
    // the ending position of the ByteBuffer
    //
    val charSet = decoder.charset()
    val resBB = charSet.encode(sb.toString())
    
    val resNumBytes = resBB.limit() // TODO: Pretty sure limit is better than length
    
    // Calculate the new ending position of the ByteBuffer
    if (endPos != -1) {
      endBitPosA =  bitOffset + (resNumBytes * 8)
    } else {
      endPos = resBB.limit()
      endBitPosA = (resBB.limit() << 3)
    }
    var endBitPosDelimA: Long = endBitPosA
    
    if (endPosDelim != -1){
      endBitPosDelimA = bitOffset + (resNumBytes * 8)
    } 
    println("ENDPOS_GET_DELIMITER: " + endPos)
    println("GET_DELIMITER - CB: " + sb.toString() + " EndBitPos: " + endBitPosA)
    println("===\nEND_GET_DELIMITER!\n===\n")
    
    if (endPos != -1 && endPosDelim != -1){ (cb.subSequence(endPos, endPosDelim).toString(), endBitPosA, endBitPosDelimA, theState, theDelimiter) }
    else { (cb.toString(), endBitPosA, endBitPosDelimA, theState, theDelimiter) }
  }

  def getInt(bitPos: Long, order: java.nio.ByteOrder) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    bb.order(order)
    bb.getInt(bytePos)
  }

  def getDouble(bitPos: Long, order: java.nio.ByteOrder) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    bb.order(order)
    val double = bb.getDouble(bytePos)
    double
  }

  def getFloat(bitPos: Long, order: java.nio.ByteOrder) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    bb.order(order)
    bb.getFloat(bytePos)
  }
}

