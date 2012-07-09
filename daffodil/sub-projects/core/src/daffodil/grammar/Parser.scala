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
import scala.collection.mutable.Queue
import scala.util.matching.Regex
import stringsearch.constructs._
import stringsearch.constructs.EscapeScheme._
import daffodil.util._
import daffodil.exceptions.ThrowsSDE


/**
 * Encapsulates lower-level parsing with a uniform interface
 */
trait Parser {
  def parse(pstate: PState): PState

  // TODO: other methods for things like asking for the ending position of something
  // which would enable fixed-length formats to skip over data and not parse it at all.
}

// No-op, in case an optimization lets one of these sneak thru. 
// TODO: make this fail, and test optimizer sufficiently to know these 
// do NOT get through.
class EmptyGramParser extends Parser {
  def parse(pstate: PState) = pstate
}

class ErrorParser extends Parser {
  def parse(pstate: PState): PState = Assert.abort("Error Parser")
  override def toString = "Error Parser"
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

class GeneralParseFailure(msg: String) extends Diagnostic {
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
  val diagnostics: List[Diagnostic]) {
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
  def failed(msg: => String): PState =
    failed(new GeneralParseFailure(msg))
  def failed(failureDiagnostic: Diagnostic) =
    new PState(inStream, bitPos, bitLimit, charPos, charLimit, parent, variableMap, target, namespaces, new Failure(failureDiagnostic.getMessage), groupIndexStack, childIndexStack, arrayIndexStack, failureDiagnostic :: diagnostics)

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
      if (sizeHint != -1) new InStreamFromByteChannel(rootElemDecl, input, sizeHint)
      else new InStreamFromByteChannel(rootElemDecl, input)
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

class InStreamFromByteChannel(context : ElementBase, in: DFDL.Input, sizeHint: Long = 1024 * 128) extends InStream with Logging { // 128K characters by default.
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
    tooSmall.foreach(b => { bb.put(b) })
    bb.position(0)
  } else {
    // Buffer is sufficiently sized
    bb.flip()
  }

  // System.err.println("InStream byte count is " + count)
  // note, our input data might just be empty string, in which case count is zero, and that's all legal.
  def fillCharBuffer(cb: CharBuffer, bitOffset: Long, decoder: CharsetDecoder): Long = {
    context.subset(bitOffset % 8 == 0, "characters must begin on byte boundaries")
    val byteOffsetAsLong = (bitOffset >> 3)
    context.subset(byteOffsetAsLong <= Int.MaxValue, "maximum offset (in bytes) cannot exceed Int.MaxValue")
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
    log(Debug("Decode Error1: " + cr1.toString()))
    if (cr1 != CoderResult.UNDERFLOW) {
      if (cr1 == CoderResult.OVERFLOW) {
        // it's ok. It just means we've satisfied the char buffer.
      } else // for some parsing, we need to know we couldn't decode, but this is expected behavior.
        return -1L // Assert.abort("Something went wrong while decoding characters: CoderResult = " + cr1)   
    }
    val cr2 = decoder.flush(cb)
    log(Debug("Decode Error2: " + cr2.toString()))
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

  import SearchResult._
  import stringsearch.delimiter._
  def fillCharBufferUntilDelimiterOrEnd(cb: CharBuffer, bitOffset: Long,
    decoder: CharsetDecoder, separators: Set[String], terminators: Set[String],
    es: EscapeSchemeObj): (String, Long, SearchResult, Delimiter) = {

    val me: String = "fillCharBufferUntilDelimiterOrEnd - "
    log(Debug("BEG_fillCharBufferUntilDelimiterOrEnd"))

    val byteOffsetAsLong = (bitOffset >> 3)
    val byteOffset = byteOffsetAsLong.toInt

    log(Debug(me + "Starting at byteOffset: " + byteOffset))
    log(Debug(me + "Starting at bitOffset: " + bitOffset))

    var (endBitPosA: Long, state) = fillCharBufferMixedData(cb, bitOffset, decoder)
    var sb: StringBuilder = new StringBuilder // To keep track of the searched text
    val dSearch = new DelimSearcher with ConsoleLogger
    var buf = cb

    if (endBitPosA == -1L) {
      log(Debug(me + "Failed, reached end of buffer."))
      log(Debug("END_fillCharBufferUntilDelimiterOrEnd - ERR!"))
      //return (cb.toString(), -1L, SearchResult.NoMatch, null)
      return (null, -1L, SearchResult.EOD, null)
    }

    //println("START_CB: " + cb.toString())

    dSearch.setEscapeScheme(es)

    separators foreach {
      x => dSearch.addSeparator(x)
    }

    terminators foreach {
      x => dSearch.addTerminator(x)
    }

    var (theState, result, endPos, endPosDelim, theDelimiter) = dSearch.search(buf, 0)

    if (theDelimiter == null) { (cb.toString(), -1L, SearchResult.NoMatch, null) }

    if (theDelimiter != null) { log(Debug(me + "Reached " + theDelimiter)) }

    if (theState == SearchResult.FullMatch) {
      sb.append(result)
    }
    var EOF: Boolean = false // Did we run off the end of the buffer?

    if (buf.toString().length == 0) { EOF = true } // Buffer was empty to start, nothing to do.

    // Proceed until we encounter a FullMatch or EOF
    while ((theState == SearchResult.NoMatch || theState == SearchResult.PartialMatch) && endBitPosA != -1 && !EOF) {
      log(Debug("fillCharBufferUntilDelimiterOrEnd - LOOP!"))
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

    val resNumBytes = resBB.limit() // TODO: Pretty sure limit is better than length

    // Calculate the new ending position of the ByteBuffer
    if (endPos != -1) {
      endBitPosA = bitOffset + (resNumBytes * 8)
    } else {
      endPos = resBB.limit()
      endBitPosA = (resBB.limit() << 3)
    }

    log(Debug(me + "Ended at byteOffset: " + (resNumBytes + byteOffset)))
    log(Debug(me + "Ended at bitOffset: " + endBitPosA))

    log(Debug("END_fillCharBufferUntilDelimiterOrEnd - CB: " + sb.toString() + ", EndBitPos: " + endBitPosA))
    log(Debug("END_fillCharBufferUntilDelimiterOrEnd"))
    (sb.toString(), endBitPosA, theState, theDelimiter)
  }

  def fillCharBufferWithPatternMatch(cb: CharBuffer, bitOffset: Long, decoder: CharsetDecoder,
    pattern: String, es: EscapeSchemeObj): (String, Long, SearchResult) = {
    log(Debug("===\nSTART_FILL!\n===\n"))
    val byteOffsetAsLong = (bitOffset >> 3)
    val byteOffset = byteOffsetAsLong.toInt

    var (endBitPosA: Long, state) = fillCharBufferMixedData(cb, bitOffset, decoder)
    var sb: StringBuilder = new StringBuilder // To keep track of the searched text
    val dSearch = pattern.r
    var buf = cb

    if (endBitPosA == -1L) {
      log(Debug("Failed, reached end of buffer."))
      //return (cb.toString(), -1L, SearchResult.NoMatch, "")
      return (null, -1L, SearchResult.EOD)
    }

    log(Debug("START_CB: " + cb.toString()))
    log(Debug("CB_" + cb.toString() + "_END_CB"))

    var (theState, endPos, result) = dSearch findPrefixMatchOf buf match {
      case Some(mch) => (SearchResult.FullMatch, mch.end.toLong, mch.matched)
      // Initial/Default values if not matched
      // TODO: What should result be if string not found?
      case None => (SearchResult.NoMatch, -1L, "")
    }

    // TODO: What is this line for?
    var imBuffer = CharBuffer.allocate(buf.capacity)

    if (theState == SearchResult.FullMatch) {
      sb.append(result)
    }
    var EOF: Boolean = false

    if (buf.toString().length == 0) { EOF = true } // Buffer was empty to start, nothing to do.

    // Buffer not big enough
    // TODO: There should be a way to pre-allocate the buffer to be big enough or at least get a better estimate than
    //       1000
    // Proceed until we encounter a FullMatch or EOF
    while ((theState == SearchResult.NoMatch || theState == SearchResult.PartialMatch) && endBitPosA != -1 && !EOF) {
      // TODO: Clear??  You mean copy it again?
      buf.clear()
      buf = CharBuffer.allocate(buf.length * 2)

      val fillState = fillCharBufferMixedData(buf, bitOffset, decoder)
      endBitPosA = fillState._1
      EOF = fillState._2

      var (state2, endPos2, result2) = dSearch findPrefixMatchOf buf match {
        case Some(mch) => (SearchResult.FullMatch, mch.end.toLong, mch.matched)
        // TODO: What should result be if string not found?
        case None => (SearchResult.NoMatch, -1L, "")
      }
      theState = state2
      endPos = endPos2

      if (theState != SearchResult.PartialMatch) {
        sb.append(result2)
      }
    }

    // Encode the found string in order to calculate
    // the ending position of the ByteBuffer
    //
    val charSet = decoder.charset()
    val resBB = charSet.encode(sb.toString())

    log(Debug("ENDPOS_FillCharBuffer: " + endPos))

    //    // Calculate the new ending position of the ByteBuffer
    //    if (endPos != -1) {
    //      endBitPosA = (endPos << 3) + bitOffset
    //    } else {
    //      endBitPosA = (resBB.limit() << 3) + bitOffset
    //    }

    val resNumBytes = resBB.limit() // TODO: Pretty sure limit is better than length

    // Calculate the new ending position of the ByteBuffer
    if (endPos != -1) {
      endBitPosA = bitOffset + (resNumBytes * 8)
    } else {
      endPos = resBB.limit()
      endBitPosA = (resBB.limit() << 3)
    }
    log(Debug("FILL - CB: " + sb.toString() + ", EndBitPos: " + endBitPosA))
    log(Debug("===\nEND_FILL!\n===\n"))
    (sb.toString(), endBitPosA, theState)
  }

  def decodeNBytes(N: Int, array: Array[Byte], decoder: CharsetDecoder): CharBuffer = {
    val list: Queue[Byte] = Queue.empty
    for (i <- 0 to N - 1) {
      list += array(i).toByte
    }
    val cb = decoder.decode(ByteBuffer.wrap(list.toArray[Byte]))
    cb
  }

  def decodeUntilFail(bytesArray: Array[Byte], decoder: CharsetDecoder, endByte: Int): (CharBuffer, Int) = {
    var cbFinal: CharBuffer = CharBuffer.allocate(1)
    var cbPrev: CharBuffer = CharBuffer.allocate(1)
    var numBytes: Int = 1
    while (numBytes <= endByte) {
      try {
        cbPrev = decodeNBytes(numBytes, bytesArray, decoder)
        cbFinal = cbPrev
      } catch {
        case e: Exception => log(Debug("Exception in decodeUntilFail: " + e.toString()))
      }
      numBytes += 1
    }
    (cbFinal, (numBytes - 1))
  }

  // Fills the CharBuffer with as many bytes as can be decoded successfully.
  //
  def fillCharBufferMixedData(cb: CharBuffer, bitOffset: Long, decoder: CharsetDecoder): (Long, Boolean) = {
    context.subset(bitOffset % 8 == 0, "characters must begin on byte boundaries")
    val byteOffsetAsLong = (bitOffset >> 3)
    context.subset(byteOffsetAsLong <= Int.MaxValue, "maximum offset (in bytes) cannot exceed Int.MaxValue")
    val byteOffset = byteOffsetAsLong.toInt
    val me: String = "fillCharBufferMixedData - "
    // 
    // Note: not thread safe. We're depending here on the byte buffer being private to us.
    //
    log(Debug(me + "Start at byteOffset " + byteOffset))
    log(Debug(me + "byteBuffer limit: " + bb.limit()))

    if (byteOffset >= bb.limit()) {
      // We are at end, nothing more to do.
      log(Debug(me + "byteOffset >= limit! Nothing more to do."))
      return (-1L, true)
    }

    bb.position(byteOffset) // Set byte position of ByteBuffer to the offset
    decoder.reset()

    var byteArray: Array[Byte] = new Array[Byte](bb.limit() - byteOffset)

    // Retrieve a byte array from offset to end of ByteBuffer.
    // Starts at 0, because ByteBuffer was already set to byteOffset
    // Ends at ByteBuffer limit in Bytes minus the offset
    bb.get(byteArray, 0, (bb.limit - byteOffset))

    var (result: CharBuffer, bytesDecoded: Int) = decodeUntilFail(byteArray, decoder, bb.limit())

    if (bytesDecoded == 0) { return (-1L, true) }

    log(Debug("MixedDataResult: " + result + " bytesDecoded: " + bytesDecoded))

    cb.clear()
    cb.append(result)

    cb.flip() // so the caller can now read the sb.

    val endBytePos = byteOffset + bytesDecoded

    log(Debug(me + "Ended at byte pos " + endBytePos))

    var EOF: Boolean = bb.limit() == bb.position()

    bb.position(0) // prevent anyone depending on the buffer position across calls to any of the InStream methods.
    val endBitPos: Long = endBytePos << 3

    log(Debug(me + "Ended at bit pos " + endBitPos))

    (endBitPos, EOF)
  }

  // Read the delimiter if possible off of the ByteBuffer
  //
  def getDelimiter(cb: CharBuffer, bitOffset: Long,
    decoder: CharsetDecoder, separators: Set[String], terminators: Set[String],
    es: EscapeSchemeObj): (String, Long, Long, SearchResult, Delimiter) = {

    log(Debug("BEG_getDelimiter"))

    val me: String = "getDelimiter - "

    log(Debug(me + "Looking for: " + separators + " AND " + terminators))

    val byteOffsetAsLong = (bitOffset >> 3)

    val byteOffset = byteOffsetAsLong.toInt

    log(Debug(me + "ByteOffset: " + byteOffset + " BitOffset: " + bitOffset))

    var (endBitPos: Long, state) = fillCharBufferMixedData(cb, bitOffset, decoder)
    var endBitPosA: Long = endBitPos

    if (endBitPos == -1L) {
      log(Debug(me + "Failed, reached end of buffer."))
      log(Debug("END_getDelimiter - End of Buffer!"))
      return (cb.toString(), -1L, -1L, SearchResult.NoMatch, null)
    }

    var sb: StringBuilder = new StringBuilder // To keep track of the searched text
    val dSearch = new DelimSearcher with Logging
    var buf = cb

    dSearch.setEscapeScheme(es)

    separators foreach { x => dSearch.addSeparator(x) }

    terminators foreach { x => dSearch.addTerminator(x) }

    var (theState, result, endPos, endPosDelim, theDelimiter) = dSearch.search(buf, 0)

    if (theDelimiter == null) { return (cb.toString(), -1L, -1L, SearchResult.NoMatch, null) }

    log(Debug("theDelimiter: " + theDelimiter.typeDef))

    if (theDelimiter.typeDef == DelimiterType.Terminator) { return (cb.toString(), -1L, -1L, SearchResult.NoMatch, null) }

    if (theState == SearchResult.FullMatch) {
      sb.append(result)
    }

    var EOF: Boolean = false // Flag to indicate if we ran out of data to fill CharBuffer with

    if (buf.toString().length == 0) { EOF = true } // Buffer was empty to start, nothing to do

    // Proceed until we encounter a FullMatch or EOF (we ran out of data)
    while ((theState == SearchResult.NoMatch || theState == SearchResult.PartialMatch) && endBitPosA != -1 && !EOF) {
      buf.clear()
      buf = CharBuffer.allocate(buf.length() * 2)

      val fillState = fillCharBufferMixedData(buf, bitOffset, decoder)
      endBitPosA = fillState._1
      EOF = fillState._2 // Determine if we ran out of data to fill the CharBuffer with

      var (state2, result2, endPos2, endPosDelim2, theDelimiter2) = dSearch.search(buf, endPosDelim, false)

      theState = state2 // Determine if there was a Full, Partial or No Match
      endPos = endPos2 // Start of delimiter
      endPosDelim = endPosDelim2 // End of delimiter
      theDelimiter = theDelimiter2

      if (theState != SearchResult.PartialMatch) {
        sb.append(result2)
      }
    }

    var delimLength = endPosDelim - endPos

    if (endPosDelim == 0 && endPos == 0 && theState == SearchResult.FullMatch) { delimLength = 1 }

    // Encode the found string in order to calculate
    // the ending position of the ByteBuffer
    //
    val charSet = decoder.charset()
    val resBB = charSet.encode(sb.toString())

    val resNumBytes = resBB.limit() // TODO: Pretty sure limit is better than length

    // Calculate the new ending position of the ByteBuffer
    if (endPos != -1) {
      endBitPosA = bitOffset + (resNumBytes * 8)
    } else {
      endPos = resBB.limit()
      endBitPosA = (resBB.limit() << 3)
    }
    var endBitPosDelimA: Long = endBitPosA

    if (endPosDelim != -1) {
      endBitPosDelimA = bitOffset + (resNumBytes * 8)
    }

    log(Debug(me + "Ended at BytePos: " + (byteOffset + resNumBytes)))
    log(Debug(me + "Ended at bitPos: " + endBitPosA))
    log(Debug("END_getDelimiter"))

    if (endPos != -1 && endPosDelim != -1) { (cb.subSequence(endPos, endPosDelim).toString(), endBitPosA, endBitPosDelimA, theState, theDelimiter) }
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

