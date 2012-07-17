package daffodil.grammar

import org.jdom._
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
import junit.framework.Assert.assertTrue
import junit.framework.Assert.assertEquals

import daffodil.util._
import daffodil.exceptions.ThrowsSDE
import java.io.ByteArrayOutputStream
import scala.collection.mutable.Stack

class UnparseError(sc: SchemaComponent, ustate: UState, kind: String, args: Any*) extends ProcessingError {
  def isError = true
  def getSchemaLocations = List(sc)
  def getDataLocations = List(ustate.currentLocation)

  override def toString = {
    lazy val argsAsString = args.map { _.toString }.mkString(", ")
    //
    // Right here is where we would lookup the symbolic error kind id, and
    // choose a locale-based message string.
    //
    // For now, we'll just do an automatic English message.
    //
    val msg =
      if (kind.contains("%")) kind.format(args: _*)
      else (kind + "(%s)").format(argsAsString)
    val res = "Unparse Error: " + msg +
      "\nContext was : %s".format(sc) +
      "\nData location was preceding %s".format(ustate.currentElement)
    res
  }

  override def getMessage = toString
}

/**
 * Encapsulates lower-level unparsing with a uniform interface
 */
abstract class Unparser(val context: Term) {

  def UE(ustate: UState, str: String, args: Any*) = {
    ustate.failed(new UnparseError(context, ustate, str, args: _*))
  }

  def processingError(ustate: UState, str: String, args: Any*) =
    UE(ustate, str, args) // long form synonym

  def unparse(ustate: UState): UState

  // TODO: other methods for things like asking for the ending position of something
  // which would enable fixed-length formats to skip over data and not unparse it at all.
}

// No-op, in case an optimization lets one of these sneak thru. 
// TODO: make this fail, and test optimizer sufficiently to know these 
// do NOT get through.
class EmptyGramUnparser(context: Term = null) extends Unparser(context) {
  def unparse(uState: UState) = Assert.invariantFailed("EmptyGramUnparsers are all supposed to optimize out!")
}

class ErrorUnparser(context: Term = null) extends Unparser(context) {
  def unparse(ustate: UState): UState = Assert.abort("Error Unparser")
  override def toString = "Error Unparser"
}

class SeqCompUnparser(context: Term, p: Gram, q: Gram) extends Unparser(context) {
  Assert.invariant(!p.isEmpty && !q.isEmpty)
  val pUnparser = p.unparser
  val qUnparser = q.unparser
  def unparse(ustate: UState) = {
    val pResult = pUnparser.unparse(ustate)
    if (pResult.status == Success) {
      val qResult = qUnparser.unparse(pResult)
      qResult
    } else pResult
  }

  override def toString = pUnparser.toString + " ~ " + qUnparser.toString
}

class AltCompUnparser(context: Term, p: Gram, q: Gram) extends Unparser(context) {
  Assert.invariant(!p.isEmpty && !q.isEmpty)
  val pUnparser = p.unparser
  val qUnparser = q.unparser
  def unparse(ustate: UState) = {

    // TODO: capture current Infoset node (make a shallow copy of it)
    // restoring this later (literally, clobbering the parent to point to 
    // this copy), is the way we backtrack the side-effects on the Infoset.
    //
    // TBD: better Scala idiom for try/catch than this var stuff
    // Use of var makes this code non-thread-safe. If you can avoid var, then you
    // don't have to worry about multiple threads at all.
    var pResult: UState = null

    val numChildrenAtStart = ustate.currentElement.getChildren().length

    try {
      pResult = pUnparser.unparse(ustate)
    } catch {
      case e: Exception => {
        // TODO: we need to record the problem so that we can
        // use it as a diagnostic in case the other alternative also fails.
      }
    }

    if (pResult != null && pResult.status == Success) pResult
    else {
      // Unwind any side effects on the Infoset 
      val lastChildIndex = ustate.currentElement.getChildren().length
      if (lastChildIndex > numChildrenAtStart) {
        ustate.currentElement.removeContent(lastChildIndex - 1) // Note: XML is 1-based indexing, but JDOM is zero based
      }
      // TODO: check for discriminator evaluated to true.
      // If so, then we don't run the next alternative, we
      // consume this discriminator status result (so it doesn't ripple upward)
      // and return the failed state.
      //
      val qResult = qUnparser.unparse(ustate)
      qResult
    }
  }

  override def toString = "(" + pUnparser.toString + " | " + qUnparser.toString + ")"
}

class RepExactlyNUnparser(context: Term, n: Long, r: => Gram) extends Unparser(context) {
  Assert.invariant(!r.isEmpty)
  val rUnparser = r.unparser

  def unparse(uState: UState): UState = {
    val intN = n.toInt // TODO: Ints aren't big enough for this.
    var pResult = uState
    1 to intN foreach { _ =>
      {
        val pNext = rUnparser.unparse(pResult)
        if (pNext.status != Success) return pNext
        pResult = pNext
      }
    }
    pResult
  }

  override def toString = "RepExactlyNUnparser(" + rUnparser.toString + ")"
}

class RepUnboundedUnparser(context: Term, r: => Gram) extends Unparser(context) {
  Assert.invariant(!r.isEmpty)
  val rUnparser = r.unparser

  def unparse(uState: UState): UState = {
    var pResult = uState
    while (pResult.status == Success) {
      val cloneNode = pResult.captureJDOM
      val pNext = rUnparser.unparse(pResult)
      if (pNext.status != Success) {
        pResult.restoreJDOM(cloneNode)
        log(Debug("Failure suppressed."))
        return pResult
      }
      pResult = pNext
    }
    Assert.invariantFailed("Unbounded loop terminated wrong")
  }

  override def toString = "RepUnboundedUnparser(" + rUnparser.toString + ")"
}

case class DummyUnparser(sc: PropertyMixin) extends Unparser(null) {
  def unparse(ustate: UState): UState = Assert.abort("Unparser for " + sc + " is not yet implemented.")
  override def toString = if (sc == null) "Dummy[null]" else "Dummy[" + sc.detailName + "]"
}

class GeneralUnparseFailure(msg: String) extends Diagnostic {
  def isError() = true
  def getSchemaLocations() = Nil
  def getDataLocations() = Nil
  def getMessage() = msg
}

class DataLocUnparse(elem: org.jdom.Element, outStream: OutStream) extends DataLocation {
  override def toString() = "Location in infoset " + elem.getName() + " of Stream: " + outStream
}

/**
 * An unparser takes a state, and returns an updated state
 *
 * The fact that there are side-effects/mutations on parts of the state
 * enables us to reuse low-level java primitives that mutate streams.
 *
 * The goal however, is to hide that fact so that the only places that have to
 * know are the places doing the mutation, and the places rolling them back
 * which should be isolated to the alternative unparser.
 */
class UState(
  val outStream: OutStream,
  val infoset: org.jdom.Document,
  val root: org.jdom.Element,
  val currentElement: org.jdom.Element,
  val rootName: String,

  val bitPos: Long,
  val bitLimit: Long,
  val charPos: Long,
  val charLimit: Long,
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
  def currentLocation: DataLocation = new DataLocUnparse(currentElement, outStream)

  /**
   * Convenience functions for creating a new state, changing only
   * one or a related subset of the state components.
   */
  //  def withOutStream(outStream: OutStream, status: ProcessorResult = Success) =
  //    new UState(outStream, infoset, root, currentElement, rootName, bitPos, bitLimit, charPos, charLimit,variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, diagnostics)
  //  def withPos(bitPos: Long, charPos: Long, status: ProcessorResult = Success) =
  //    new UState(outStream, infoset, root, currentElement, rootName, bitPos, bitLimit, charPos, charLimit, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, diagnostics)
  def withCurrent(currentElement: org.jdom.Element, status: ProcessorResult = Success) =
    new UState(outStream, infoset, root, currentElement, rootName, bitPos, bitLimit, charPos, charLimit, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, diagnostics)
  //  def withVariables(variableMap: VariableMap, status: ProcessorResult = Success) =
  //    new UState(outStream, infoset, root, currentElement, rootName, bitPos, bitLimit, charPos, charLimit, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, diagnostics)
  def withGroupIndexStack(groupIndexStack: List[Long], status: ProcessorResult = Success) =
    new UState(outStream, infoset, root, currentElement, rootName, bitPos, bitLimit, charPos, charLimit, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, diagnostics)
  def withChildIndexStack(childIndexStack: List[Long], status: ProcessorResult = Success) =
    new UState(outStream, infoset, root, currentElement, rootName, bitPos, bitLimit, charPos, charLimit, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, diagnostics)
  def withArrayIndexStack(arrayIndexStack: List[Long], status: ProcessorResult = Success) =
    new UState(outStream, infoset, root, currentElement, rootName, bitPos, bitLimit, charPos, charLimit, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, diagnostics)
  def failed(msg: => String): UState =
    failed(new GeneralUnparseFailure(msg))
  def failed(failureDiagnostic: Diagnostic) =
    new UState(outStream, infoset, root, currentElement, rootName, bitPos, bitLimit, charPos, charLimit, variableMap, target, namespaces, new Failure(failureDiagnostic.getMessage), groupIndexStack, childIndexStack, arrayIndexStack, failureDiagnostic :: diagnostics)

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
    infoset.getContentSize()
  }

  def restoreJDOM(previousContentSize: Int) = {
    for (i <- previousContentSize until infoset.getContentSize()) {
      infoset.removeContent(i)
    }
    this
    //    val pp = parent.getParent().asInstanceOf[org.jdom.Element] // Parent's Parent.
    //    val pi :: ppi :: rest = childIndexStack
    //    pp.setContent(ppi.toInt, newElem)
    //    newElem
  }
}

object UState {

  /**
   * Initialize the state block given our OutStream, a root element declaration, and an infoset.
   */
  def createInitialState(rootElemDecl: GlobalElementDecl, out: OutStream, infoset: org.jdom.Document): UState = {
    val elem = infoset.getContent()
    //TODO: give appropriate error if not single root element in infoset--infoset validation done elsewhere?
    assertTrue(elem.size() == 1)
    val root = elem(0).asInstanceOf[org.jdom.Element]
    val rootName = root.getName()

    val variables = new VariableMap()
    val targetNamespace = rootElemDecl.schemaDocument.targetNamespace
    val namespaces = new Namespaces()
    val status = Success
    val groupIndexStack = Nil
    val childIndexStack = Nil
    val arrayIndexStack = Nil
    val diagnostics = Nil
    val newState = new UState(out, infoset, root, root, rootName, 0, -1, 0, -1, variables, targetNamespace, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, diagnostics)
    newState
  }

  /**
   * For testing it is convenient to just hand it strings for data.
   */
  def createInitialState(rootElemDecl: GlobalElementDecl, data: String, document: org.jdom.Document): UState = {
    val out = Compiler.stringToWritableByteChannel(data)
    createInitialState(rootElemDecl, out, document, data.length)
  }

  /**
   * Construct our OutStream object and initialize the state block.
   */
  def createInitialState(rootElemDecl: GlobalElementDecl, output: DFDL.Output, document: org.jdom.Document, sizeHint: Long = -1): UState = {
    val outStream =
      if (sizeHint != -1) new OutStreamFromByteChannel(rootElemDecl, output, sizeHint)
      else new OutStreamFromByteChannel(rootElemDecl, output)
    createInitialState(rootElemDecl, outStream, document)
  }
}

/**
 * Encapsulates the I/O as an abstraction that works something like a java.nio.CharBuffer, but a
 * bit more specialized for DFDL needs.
 */
trait OutStream {
  /**
   * These return a value of the appropriate type, or they throw
   * an exception when there is no more data, or if the offset is past the end of data,
   * or if the offset exceeds implementation capacity such as for moving backwards in
   * the data beyond buffering capacity.
   */
  //  def getBinaryLong(bitOffset : Long,  isBigEndian : Boolean) : Long
  //  def getBinaryInt(bitOffset : Long,  isBigEndian : Boolean) : Int

  def fillCharBuffer(nBytes: Long, data: String, encoder: CharsetEncoder)
  def write()
  def charBufferToByteBuffer(): ByteBuffer

  def getData(): String
  def setData(str: String)

  def setDelimiters(separators: Set[String], terminators: Set[String])
}

/*
 * Not thread safe. We're depending on the CharBuffer being private to us.
 */
class OutStreamFromByteChannel(context: ElementBase, outStream: DFDL.Output, sizeHint: Long = 1024 * 128) extends OutStream with Logging { // 128K characters by default.
  val maxCharacterWidthInBytes = 4 //FIXME: worst case. Ok for testing. Don't use this pessimistic technique for real data.
  var cbuf = CharBuffer.allocate(maxCharacterWidthInBytes * sizeHint.toInt) // FIXME: all these Int length limits are too small for large data blobs
  var encoder: CharsetEncoder = null //FIXME

  /*
   * Moves data to CharBuffer, resizing as necessary.
   */
  def fillCharBuffer(nBytes: Long, data: String, enc: CharsetEncoder) = {
    encoder = enc
    cbuf.clear() //remove old data from previous element

    setData(data)
  }

  /*
   * Writes the delimiters to CharBuffer.
   */
  def setDelimiters(separators: Set[String], terminators: Set[String]) {
    setLoggingLevel(LogLevel.Debug)
    val me: String = "setDelimiters - "
    log(Debug(me + "Inserting separators: " + separators + " and terminators: " + terminators))

    //could just do in CharBuffer
    var sb: StringBuilder = new StringBuilder(cbuf.toString())

    //TODO: this is oversimplified
    //TODO: also always selects first delimiter from Seq
    if (!terminators.isEmpty) {
      sb.append(terminators.head)
    }
    cbuf.clear()
    setData(sb.toString())
  }

  /*
   * Writes unparsed data in CharBuffer to outputStream.
   */
  def write() {
    val bbuf = charBufferToByteBuffer()
    outStream.write(bbuf)
  }

  /*
   * Takes unparsed data from CharBuffer and encodes it in ByteBuffer
   */
  def charBufferToByteBuffer(): ByteBuffer = {
    val bbuf = ByteBuffer.allocate(cbuf.length() * maxCharacterWidthInBytes)
    encoder.reset()

    val cr1 = encoder.encode(cbuf, bbuf, true) // true means this is all the input you get.

    log(Debug("Decode Error1: " + cr1.toString()))
    if (cr1 != CoderResult.UNDERFLOW) {
      if (cr1 == CoderResult.OVERFLOW) {
        // it's ok. It just means we've satisfied the byte buffer.
      } //else // for some unparsing, we need to know we couldn't decode, but this is expected behavior.
      // Assert.abort("Something went wrong while decoding characters: CoderResult = " + cr1)   
    }
    val cr2 = encoder.flush(bbuf)
    log(Debug("Decode Error2: " + cr2.toString()))
    if (cr2 != CoderResult.UNDERFLOW) {
      // Something went wrong
      // Assert.abort("Something went wrong while decoding characters: CoderResult = " + cr2) 
      // FIXME: proper handling of errors. Some of which are 
      // to be suppressed, other converted, others skipped, etc. 
    }
    bbuf.flip() // so the caller can read
    bbuf
  }

  def getData(): String = {
    cbuf.toString
  }

  def setData(str: String) {
    var isTooSmall = true
    while (isTooSmall) {
      try { //writing all data to char buffer
        cbuf.put(str, 0, str.length())
        cbuf.flip() // prevent anyone depending on the buffer position across calls to any of the OutStream methods.
        isTooSmall = false
      } catch { //make sure buffer was not read to capacity
        case e: Exception =>
          cbuf = CharBuffer.allocate(cbuf.capacity() * 4)
      }
    }
  }
}

