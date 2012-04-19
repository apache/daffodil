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

import scala.collection.JavaConversions._

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
  val arrayIndexStack: List[Long]) {
  def bytePos = bitPos >> 3
  def whichBit = bitPos % 8
  def groupPos = groupIndexStack.head
  def childPos = childIndexStack.head

  /**
   * Convenience functions for creating a new state, changing only
   * one or a related subset of the state components to a new one.
   */
  def withInStream(inStream: InStream, status: ProcessorResult = Success) =
    new PState(inStream, bitPos, bitLimit, charPos, charLimit, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack)
  def withPos(bitPos: Long, charPos: Long, status: ProcessorResult = Success) =
    new PState(inStream, bitPos, bitLimit, charPos, charLimit, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack)
  def withParent(parent: org.jdom.Element, status: ProcessorResult = Success) =
    new PState(inStream, bitPos, bitLimit, charPos, charLimit, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack)
  def withVariables(variableMap: VariableMap, status: ProcessorResult = Success) =
    new PState(inStream, bitPos, bitLimit, charPos, charLimit, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack)
  def withGroupIndexStack(groupIndexStack: List[Long], status: ProcessorResult = Success) =
    new PState(inStream, bitPos, bitLimit, charPos, charLimit, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack)
  def withChildIndexStack(childIndexStack: List[Long], status: ProcessorResult = Success) =
    new PState(inStream, bitPos, bitLimit, charPos, charLimit, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack)
  def withArrayIndexStack(arrayIndexStack: List[Long], status: ProcessorResult = Success) =
    new PState(inStream, bitPos, bitLimit, charPos, charLimit, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack)
  def failed(msg: => String) =
    new PState(inStream, bitPos, bitLimit, charPos, charLimit, parent, variableMap, target, namespaces, new Failure(msg), groupIndexStack, childIndexStack, arrayIndexStack)

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
    val newState = new PState(inStream, 0, -1, 0, -1, doc, variables, targetNamespace, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack)
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
  def createInitialState(rootElemDecl: GlobalElementDecl, input: DFDL.Input, size: Long = -1): PState = {
    val inStream =
      if (size != -1) new InStreamFromByteChannel(input, size)
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
  
  def getInt(bitPos : Long, order : java.nio.ByteOrder) : Int
  def getDouble(bitPos : Long, order : java.nio.ByteOrder) : Double
  def getFloat(bitPos : Long, order : java.nio.ByteOrder) : Float

  // def fillCharBufferUntilDelimiterOrEnd
}

class InStreamFromByteChannel(in: DFDL.Input, size: Long = 1024 * 128) extends InStream { // 128K characters by default.
  val maxCharacterWidthInBytes = 4 // worst case. Ok for testing. Don't use this pessimistic technique for real data.
  val bb = ByteBuffer.allocate(maxCharacterWidthInBytes * size.toInt) // FIXME: all these Int length limits are too small for large data blobs
  val count = in.read(bb) // just pull it all into the byte buffer
  bb.flip()

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
    bb.position(byteOffset)
    decoder.reset()
    val cr1 = decoder.decode(bb, cb, true) // true means this is all the input you get.
    if (cr1 != CoderResult.UNDERFLOW) {
      if (cr1 == CoderResult.OVERFLOW) {
        // it's ok. It just means we've satisfied the char buffer.
      } else // for some parsing, we need to know we couldn't decode, but this is expected behavior.
        return -1L // Assert.abort("Something went wrong while decoding characters: CoderResult = " + cr1)   
    }
    val cr2 = decoder.flush(cb)
    if (cr2 != CoderResult.UNDERFLOW) {
      // Something went wrong
      return -1L // Assert.abort("Something went wrong while decoding characters: CoderResult = " + cr2) 
      // FIXME: proper handling of errors. Some of which are 
      // to be suppressed, other converted, others skipped, etc. 
    }
    cb.flip() // so the caller can now read the cb.

    val endBytePos = bb.position()
    bb.position(0) // prevent anyone depending on the buffer position across calls to any of the InStream methods.
    val endBitPos: Long = endBytePos << 3
    endBitPos
  }

  def fillCharBufferUntilDelimiterOrEnd(cb: CharBuffer, bitOffset: Long, decoder: CharsetDecoder, delimiters: Set[String]): Long = {
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
        return -1L // Assert.abort("Something went wrong while decoding characters: CoderResult = " + cr1)   
    }
    val cr2 = decoder.flush(cb)
    if (cr2 != CoderResult.UNDERFLOW) {
      // Something went wrong
      return -1L // Assert.abort("Something went wrong while decoding characters: CoderResult = " + cr2) 
      // FIXME: proper handling of errors. Some of which are 
      // to be suppressed, other converted, others skipped, etc. 
    }
    cb.flip() // so the caller can now read the cb.

    val endBytePos = bb.position()
    bb.position(0) // prevent anyone depending on the buffer position across calls to any of the InStream methods.
    val endBitPos: Long = endBytePos << 3
    endBitPos
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

