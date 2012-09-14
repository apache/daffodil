package daffodil.processors

import org.jdom._
import daffodil.xml._
import daffodil.processors._
import daffodil.grammar._
import daffodil.compiler._
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
import java.io.ByteArrayInputStream
import scala.collection.mutable.Stack
import daffodil.debugger.Debugger
import daffodil.util.Misc._
import java.io.InputStreamReader
import java.io.BufferedReader

abstract class ProcessingError extends Exception with DiagnosticImplMixin

class ParseError(sc : SchemaComponent, val pstate : Option[PState], kind : String, args : Any*) 
extends ProcessingError {
  def isError = true
  def getSchemaLocations = List(sc)
  def getDataLocations = pstate.map { _.currentLocation }.toList
  
  override def toString = {
    lazy val argsAsString = args.map { _.toString }.mkString(", ")
    //
    // Right here is where we would lookup the symbolic error kind id, and
    // choose a locale-based message string.
    //
    // For now, we'll just do an automatic English message.
    //
    val msg =
      if (kind.contains("%")) kind.format(args : _*)
      else (kind + "(%s)").format(argsAsString)
    val res = "Parse Error: " + msg +
      "\nContext was : %s".format(sc) +
      pstate.map{ ps => "\nData location was preceding %s".format(ps.currentLocation) }.getOrElse("(no data location)")
    res
  }

  override def getMessage = toString
}

class AssertionFailed(sc: SchemaComponent, state : PState, msg : String) 
extends ParseError(sc, Some(state), "Assertion failed. %s", msg)


class AlternativeFailed(sc : SchemaComponent, state : DFDL.State, val errors : Seq[Diagnostic]) extends ProcessingError {
  def isError = true
  def getSchemaLocations = List(sc)
  def getDataLocations = List(state.currentLocation)
  
  override def toString() = {
    val msg = "Alternative failed: Reason(s): " + errors.map{_.toString}.mkString("\n")
    msg
  }
}

class AltParseFailed(sc : SchemaComponent, state : DFDL.State,
  val p : Diagnostic, val q : Diagnostic) extends ProcessingError {
  def isError = true
  def getSchemaLocations = p.getSchemaLocations ++ q.getSchemaLocations
  def getDataLocations = {
    // both should have the same starting location if they are alternatives.
    Assert.invariant(p.getDataLocations == q.getDataLocations)
    p.getDataLocations
  }
  
  override def toString() = {
    val msg = p.toString + "\n" + q.toString
    msg
  }

}

/**
 * Encapsulates lower-level parsing with a uniform interface
 */
abstract class Parser(val context : SchemaComponent) extends Logging {
  
  def PE(pstate : PState, s : String, args : Any*) = {
    pstate.failed(new ParseError(context, Some(pstate), s, args : _*))
  }


  def processingError(state : PState, str : String, args : Any*) =
    PE(state, str, args) // long form synonym

  protected def parse(pstate : PState) : PState
  
  final def parse1(pstate : PState, context : SchemaComponent) : PState = {
    Debugger.before(pstate, this)
    val afterState = parse(pstate)
    Debugger.after(pstate, afterState, this)
    afterState
  }

  // TODO: other methods for things like asking for the ending position of something
  // which would enable fixed-length formats to skip over data and not parse it at all.
}


/**
 * Mix this into parsers that have deep algorithms that are spread over multiple classes. 
 * 
 * These allow one to bend the rule about parsers not throwing ParseError so that
 * if you are inside a parser, but you are way down a bunch of calls away from the parser itself
 * you can throw, and it will be intercepted and proper behavior (not throwing, but returning
 * a failed status) will result.
 * 
 * Use like this:
 * <pre>
 * withParseErrorThrowing(pstate) { // something enclosing like the parser
 * ...
 *   // calls something which calls something which eventually calls
 *       PECheck(bitOffset % 8 == 0, "must be byte boundary, not bit %s", bitOffset)
 * ...
 * }
 * </pre> 
 */
trait WithParseErrorThrowing {
  
  def context : SchemaComponent
  
  /**
   * Use to check for parse errors.
   * 
   * Must be used only in the context of the withParseErrorThrowing wrapper.
   * 
   * The schema component providing the context is implicit (via def context virtual member)
   */
  def PECheck(
      testTrueMeansOK : => Boolean,
      kind : String, args : Any*) {
    Assert.usage(WithParseErrorThrowing.flag, "Must use inside of withParseErrorThrowing construct.")
    if (!testTrueMeansOK) {
      throw new ParseError(context, None, kind, args : _*)
    }
  }
  
  /**
   * Passing the context explicitly
   */
  def PECheck(contextArg: SchemaComponent,  
      testTrueMeansOK : => Boolean,
      kind : String, args : Any*) {
    Assert.usage(WithParseErrorThrowing.flag, "Must use inside of withParseErrorThrowing construct.")
    if (!testTrueMeansOK) {
      throw new ParseError(contextArg, None, kind, args : _*)
    }
  }
    
  def PE(kind : String, args : Any*) : Nothing = {
    PE(context, kind, args : _*)
  }
  
  def PE(context : SchemaComponent, kind : String, args : Any*) : Nothing = {
    Assert.usage(WithParseErrorThrowing.flag, "Must use inside of withParseErrorThrowing construct.")
    throw new ParseError(context, None, kind, args : _*)
  }
  
  
    
  
  /**
   * Wrap around parser code that wants to throw parse errors (e.g., parsers which call things which 
   * call things which detect a parse error want to throw back to this)
   * 
   * This wrapper then implements the required behavior for parsers 
   * that being returning a failed parser state.
   */
  def withParseErrorThrowing(pstate : PState)(body : => PState) : PState = {
    val saveCanThrowParseErrors = WithParseErrorThrowing.flag
    WithParseErrorThrowing.flag = true
    val result = 
      try body
      catch {
        case e : ParseError => {
          val maybePS = e.pstate
          // if there is a maybePS, then use it to create the failed state (because it 
          // is probably more specific about the failure location), otherwise
          // use the one passed as an argument. 
          val res = maybePS.map{ _.failed(e) }.getOrElse(pstate.failed(e))
          res
        }
        case e : SchemaDefinitionError => {
          val res = pstate.failed(e)
          res
        }
        //
        // Note: We specifically do not catch other exceptions here
        // On purpose. If those exist, then there's someplace that should have already caught them
        // and turned them into a thrown parse error, or a schema definition error.
        //
        // Other kinds of spontaneous throws are bugs, and we don't want to mask them by 
        // putting blanket catches in. 
        //
      }
    finally {
      WithParseErrorThrowing.flag = saveCanThrowParseErrors
    }
    result
  }
  
  /**
   * Use to check things that really are schema-definition issues, but we can't check until run-time.
   * E.g., since byteOrder might be an expression, if the expression returns neither bigEndian nor littleEndian, 
   * then it's an SDE, but we didn't know until runtime.
   * 
   * No catching for this SDE throw, since SDEs are fatal.
   */
  def SDECheck(testTrueMeansOK : => Boolean, context : SchemaComponent, pstate : PState, kind : String, args : Any*) = {
    if (!testTrueMeansOK) {
      throw new SchemaDefinitionError(Some(context), None, kind, args : _*)
    }
  }
}

/**
 * Global flag to insure we aren't throwing ParseErrors in a context that won't catch them 
 * properly.
 */
object WithParseErrorThrowing {
  var flag : Boolean = false
}

// No-op, in case an optimization lets one of these sneak thru. 
// TODO: make this fail, and test optimizer sufficiently to know these 
// do NOT get through.
class EmptyGramParser(context : Term = null) extends Parser(context) {
  def parse(pstate : PState) = Assert.invariantFailed("EmptyGramParsers are all supposed to optimize out!")
}

class ErrorParser(context : Term = null) extends Parser(context) {
  def parse(pstate : PState) : PState = Assert.abort("Error Parser")
  override def toString = "Error Parser"
}

class SeqCompParser(context : AnnotatedSchemaComponent, p : Gram, q : Gram) extends Parser(context) {
  Assert.invariant(!p.isEmpty && !q.isEmpty)
  val pParser = p.parser
  val qParser = q.parser
  def parse(pstate : PState) = {
    val pResult = pParser.parse1(pstate, context)
    if (pResult.status == Success) {
      val qResult = qParser.parse1(pResult, context)
      qResult
    } else pResult
  }

  override def toString = pParser.toString + " ~ " + qParser.toString
}

class AltCompParser(context : AnnotatedSchemaComponent, p : Gram, q : Gram) extends Parser(context) {
  Assert.invariant(!p.isEmpty && !q.isEmpty)
  val pParser = p.parser
  val qParser = q.parser
  def parse(pstate : PState) : PState = {
    val numChildrenAtStart = pstate.parent.getContent().length
    var pResult : PState =
      try {
        log(Debug("Trying choice alternative: %s", pParser))
        pParser.parse1(pstate, context)
      } catch {
        case e : Exception => {
          Assert.invariantFailed("Runtime parsers should not throw exceptions: " + e)
        }
      }
    if (pResult.status == Success) {
      log(Debug("Choice alternative success: %s", pParser))
      // Reset any discriminator. We succeeded.
      val res = if (pResult.discriminator) pResult.withDiscriminator(false)
                else pResult
          res
    }
    else {
      log(Debug("Choice alternative failed: %s", pParser))

      // Unwind any side effects on the Infoset 
      val lastChildIndex = pstate.parent.getContent().length
      if (lastChildIndex > numChildrenAtStart) {
        pstate.parent.removeContent(lastChildIndex - 1) // Note: XML is 1-based indexing, but JDOM is zero based
      }
      //
      // check for discriminator evaluated to true.
      if (pResult.discriminator == true) {
        log(Debug("Failure, but discriminator true. Additional alternatives discarded."))
        // If so, then we don't run the next alternative, we
        // consume this discriminator status result (so it doesn't ripple upward)
        // and return the failed state. 
        //
        val res = pResult.withDiscriminator(false)
        return res
      }
      
      val qResult = try {
        log(Debug("Trying choice alternative: %s", qParser))
        qParser.parse1(pstate, context)
      } catch {
        case e : Exception => {
          Assert.invariantFailed("Runtime parsers should not throw exceptions: " + e)
        }
      }
      if (qResult.status == Success) {
        log(Debug("Choice alternative success: %s", qParser))
        val res = if (qResult.discriminator) qResult.withDiscriminator(false)
        else qResult
        res
      }
      else {
        log(Debug("Choice alternative failure: %s", qParser))
        // Unwind any side effects on the Infoset 
        val lastChildIndex = pstate.parent.getContent().length
        if (lastChildIndex > numChildrenAtStart) {
          pstate.parent.removeContent(lastChildIndex - 1) // Note: XML is 1-based indexing, but JDOM is zero based
        }
        
      // check for discriminator evaluated to true. But just FYI since this is the last alternative anyway
      if (qResult.discriminator == true) {
        log(Debug("Failure, but discriminator true. (last alternative anyway)"))
      }

        // Since both alternatives failed, we create two meta-diagnostics that 
        // each indicate that one alternative failed due to the errors that occurred during
        // that attempt.

        val pAltErr = new AlternativeFailed(context, pstate, pResult.diagnostics)
        val qAltErr = new AlternativeFailed(context, pstate, qResult.diagnostics)
        val altErr = new AltParseFailed(context, pstate, pAltErr, qAltErr)

        val bothFailedResult = pstate.failed(altErr)
        log(Debug("Both AltParser alternatives failed."))

        val result = PE(bothFailedResult, "Both alternatives failed.")
        result.withDiscriminator(false)
      }
    }
  }

  override def toString = "(" + pParser.toString + " | " + qParser.toString + ")"
}



case class DummyParser(sc : PropertyMixin) extends Parser(null) {
  def parse(pstate : PState) : PState = Assert.abort("Parser for " + sc + " is not yet implemented.")
  override def toString = if (sc == null) "Dummy[null]" else "Dummy[" + sc.detailName + "]"
}

class GeneralParseFailure(msg: String) extends Throwable with DiagnosticImplMixin {
  Assert.usage(msg != null && msg != "")
  def isError() = true
  def getSchemaLocations() = Nil
  def getDataLocations() = Nil
  override def getMessage() = msg
}

class DataLoc(bitPos: Long, inStream: InStream) extends DataLocation {
  
  override def toString() = "Location (in bits) " + bitPos + " (in bytes) " + bitPos / 8 + 
  ". UTF-8 text is: (" + utf8Dump + ")" + " Data aligned to 8-bytes starting at bit " + aligned64BitsPos + " byte " + aligned64BitsPos / 8 + " (" + dump + ")" 
  
  
  def aligned64BitsPos = (bitPos >> 6) << 6
  
  def byteDump = {
    var bytes : List[Byte] = Nil
    try {
      for ( i <- 0 to 40 ) {
        bytes = inStream.getByte(aligned64BitsPos + (i * 8) , java.nio.ByteOrder.BIG_ENDIAN) +: bytes
      } 
    }
    catch {
      case e : IndexOutOfBoundsException =>
    }
    bytes.reverse.toArray
  }
  
  //val cBuf = 
  /**
   * Assumes utf-8
   */
  def utf8Dump = {
    val cb = CharBuffer.allocate(128)
    val is = new ByteArrayInputStream(byteDump) 
    val ir = new InputStreamReader(is)
    val count = ir.read(cb)
    val arr = cb.array
    val chars = for {i <- 0 to count - 1} yield arr(i)
    chars.mkString("")
    }
    
  def dump = {
    bytes2Hex(byteDump)
  }
  
  /*
   * We're at the end if an attempt to get a byte fails with an index exception
   */
  def isAtEnd : Boolean = {
    try {
      inStream.getByte(bitPos, java.nio.ByteOrder.BIG_ENDIAN)
      false
    }
    catch {
      case e : IndexOutOfBoundsException => {
      val exc = e
      true
      }
    }
    }
}

/**
 * A placeholder for holding the complete stream information so that it can be popped all at once when a new stream
 * state is created
 *
 * @param inStream Input Data Stream
 * @param bitLimit Total Bits available in given Data Stream
 * @param charLimit Total UNICODE or given Character Set Characters in given Data Stream
 * @param bitPos Current Read Position in given Data Stream
 * @param charPos Current Read Character Position in UNICODE or a given Character Set for the given Data Stream
 */
class PStateStream(val inStream: InStream, val bitLimit: Long, val charLimit: Long = -1, val bitPos: Long = 0, val charPos: Long = -1) {
  Assert.invariant(bitPos >= 0)
  def withInStream(inStream: InStream, status: ProcessorResult = Success) =
    new PStateStream(inStream, bitLimit, charLimit, bitPos, charPos)
  def withPos(bitPos: Long, charPos: Long, status: ProcessorResult = Success) =
    new PStateStream(inStream, bitLimit, charLimit, bitPos, charPos)
  def withEndBitLimit(bitLimit: Long, status: ProcessorResult = Success) =
    new PStateStream(inStream, bitLimit, charLimit, bitPos, charPos)
}
object PStateStream {
  def initialPStateStream(in : InStream, bitOffset : Long) = 
    new PStateStream(in, bitLimit = -1, bitPos = bitOffset )
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
  val inStreamStateStack: Stack[PStateStream],
  val parent: org.jdom.Parent,
  val variableMap: VariableMap,
  val target: String,
  val namespaces: Any, // Namespaces
  val status: ProcessorResult,
  val groupIndexStack: List[Long],
  val childIndexStack: List[Long],
  val arrayIndexStack: List[Long],
  val occursCountStack : List[Long],
  val diagnostics : List[Diagnostic],
  val discriminator : Boolean) extends DFDL.State {
  def bytePos = bitPos >> 3
  def whichBit = bitPos % 8
  def groupPos = if (groupIndexStack != Nil) groupIndexStack.head else -1
  def childPos = if (childIndexStack != Nil) childIndexStack.head else -1
  def arrayPos = if (arrayIndexStack != Nil) arrayIndexStack.head else -1
  def occursCount = occursCountStack.head

  def currentLocation : DataLocation = new DataLoc(bitPos, inStream)
  def inStreamState = inStreamStateStack top
  def inStream = inStreamState inStream
  def bitPos = inStreamState bitPos
  def bitLimit = inStreamState bitLimit
  def charPos = inStreamState charPos
  def charLimit = inStreamState charLimit
  def parentElement = parent.asInstanceOf[Element]
  def parentForAddContent = 
    parent.asInstanceOf[{ 
      def addContent(c: org.jdom.Content) : Unit
      def removeContent(c: org.jdom.Content) : Unit 
      }]

/**
   * Convenience functions for creating a new state, changing only
   * one or a related subset of the state components to a new one.
   */

  def withInStreamState(inStreamState: PStateStream, status: ProcessorResult = Success) =
    new PState(inStreamStateStack push(inStreamState), parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminator)
  def withInStream(inStream: InStream, status: ProcessorResult = Success) =
    new PState(inStreamStateStack push(new PStateStream(inStream, bitLimit, charLimit, bitPos, charPos)), parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminator)
  def withLastInStream(status: ProcessorResult = Success) = {
    var lastBitPos = bitPos
    var lastCharPos = if (charPos > 0) charPos else 0
    inStreamStateStack pop()
    new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminator) withPos(bitPos + lastBitPos, charPos + lastCharPos)
  }
  def withPos(bitPos: Long, charPos: Long, status: ProcessorResult = Success) = {
    var newInStreamStateStack = inStreamStateStack clone()
    newInStreamStateStack pop()
    newInStreamStateStack push(new PStateStream(inStream, bitLimit, charLimit, bitPos, charPos))
    new PState(newInStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminator)
  }
  def withEndBitLimit(bitLimit: Long, status: ProcessorResult = Success) = {
    var newInStreamStateStack = inStreamStateStack clone()
    newInStreamStateStack pop()
    newInStreamStateStack push(new PStateStream(inStream, bitLimit, charLimit, bitPos, charPos))
    new PState(newInStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminator)
  }
  def withParent(parent: org.jdom.Parent, status: ProcessorResult = Success) =
    new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminator)
  def withVariables(variableMap: VariableMap, status: ProcessorResult = Success) =
    new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminator)
  def withGroupIndexStack(groupIndexStack: List[Long], status: ProcessorResult = Success) =
    new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminator)
  def withChildIndexStack(childIndexStack: List[Long], status: ProcessorResult = Success) =
    new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminator)
  def withArrayIndexStack(arrayIndexStack: List[Long], status: ProcessorResult = Success) =
    new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminator)
  def setOccursCount(oc : Long) = 
      new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, oc :: occursCountStack.tail, diagnostics, discriminator)
  def withOccursCountStack(ocs : List[Long]) = 
      new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, ocs, diagnostics, discriminator)
  def failed(msg: => String) : PState =
    failed(new GeneralParseFailure(msg))
  def failed(failureDiagnostic: Diagnostic) =
    new PState(inStreamStateStack, parent, variableMap, target, namespaces, new Failure(failureDiagnostic.getMessage), groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, failureDiagnostic :: diagnostics, discriminator)
  def withDiscriminator(disc : Boolean) = 
     new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, disc)
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
    val s2 = s1.moveOverOneElementChildOnly
    val s3 = s2.arrayIndexStack match {
      case Nil => s2
      case hd :: tl => {
        val newArrayIndex = hd + 1
        s1.withArrayIndexStack(newArrayIndex :: tl)
      }
    }
    s3
  }
  
  def moveOverOneElementChildOnly = { 
    childIndexStack match {
      case Nil => this
      case hd :: tl => {
        val newChildIndex = hd + 1
        withChildIndexStack(newChildIndex :: tl)
      }
    }
  }
 
  def captureJDOM : Int = {
    parent.getContentSize()
  }

  def restoreJDOM(previousContentSize : Int) = {
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
  def createInitialState(rootElemDecl : GlobalElementDecl, in : InStream, bitOffset : Long) : PState = {
    val inStream = in
   
    val doc = new org.jdom.Document() // must have a jdom document to get path evaluation to work.  
    val variables = rootElemDecl.schema.schemaSet.variableMap
    val targetNamespace = rootElemDecl.schemaDocument.targetNamespace
    val namespaces = null // new Namespaces()
    val status = Success
    val groupIndexStack = Nil
    val childIndexStack = Nil
    val arrayIndexStack = Nil
    val occursCountStack = Nil
    val diagnostics = Nil
    val discriminator = false
    val initPState = PStateStream.initialPStateStream(inStream, bitOffset)
    val newState = new PState(Stack(initPState), doc, variables, targetNamespace, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminator)
    newState
  }

  /**
   * For testing it is convenient to just hand it strings for data.
   */
  def createInitialState(rootElemDecl : GlobalElementDecl, data : String, bitOffset : Long) : PState = {
    val in = Compiler.stringToReadableByteChannel(data)
    createInitialState(rootElemDecl, in, data.length, bitOffset)
  }

  /**
   * Construct our InStream object and initialize the state block.
   */
  def createInitialState(rootElemDecl : GlobalElementDecl, input : DFDL.Input, sizeHint : Long = -1, bitOffset : Long = 0) : PState = {
    val inStream =
      if (sizeHint != -1) new InStreamFromByteChannel(rootElemDecl, input, sizeHint)
      else new InStreamFromByteChannel(rootElemDecl, input)
    createInitialState(rootElemDecl, inStream, bitOffset)
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
  def fillCharBufferMixedData(cb: CharBuffer, bitOffset: Long, decoder: CharsetDecoder, endByte: Long = -1): (Long, Boolean)

  // yes we do need byte order for getByte, because the byte might not be aligned to a byte boundary,
  // that is, it might straddle byte boundaries, in which case the issue of byte order arises.
  def getByte(bitPos : Long, order : java.nio.ByteOrder) : Byte
  def getShort(bitPos : Long, order : java.nio.ByteOrder) : Short  
  def getInt(bitPos : Long, order : java.nio.ByteOrder) : Int
  def getLong(bitPos : Long, order : java.nio.ByteOrder) : Long
  
  def getDouble(bitPos : Long, order : java.nio.ByteOrder) : Double
  def getFloat(bitPos : Long, order : java.nio.ByteOrder) : Float
  
  def getByteArray(bitPos : Long, order : java.nio.ByteOrder, size: Int) : Array[Byte]

  // def fillCharBufferUntilDelimiterOrEnd
}

class InStreamFromByteChannel(val context : ElementBase, in : DFDL.Input, sizeHint : Long = 1024 * 128) 
extends InStream 
with Logging 
with WithParseErrorThrowing
{ // 128K characters by default.
  Assert.usage(sizeHint > 0)
  val maxCharacterWidthInBytes = 4 // FIXME this is worst case. Ok for testing. Can we do better than this pessimistic overestimate on size?
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
  def fillCharBuffer(cb : CharBuffer, bitOffset : Long, decoder : CharsetDecoder) : Long = {
    context.subset(bitOffset >= 0 && bitOffset % 8 == 0, "characters must begin on byte boundaries. bitOffset is %s, which is aligned %s ", bitOffset, bitOffset % 8)
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
    val endBitPos : Long = endBytePos << 3

    endBitPos
  }

  import SearchResult._
  import stringsearch.delimiter._
  def fillCharBufferUntilDelimiterOrEnd(cb: CharBuffer, bitOffset: Long, 
      decoder: CharsetDecoder, separators: Set[String], terminators: Set[String],
      es: EscapeSchemeObj): (String, Long, SearchResult, Delimiter) = {
   // setLoggingLevel(LogLevel.Debug)
    
    val me: String = "fillCharBufferUntilDelimiterOrEnd - "
    log(Debug("BEG_fillCharBufferUntilDelimiterOrEnd"))

    val byteOffsetAsLong = (bitOffset >> 3)
    val byteOffset = byteOffsetAsLong.toInt

    log(Debug(me + "Starting at byteOffset: " + byteOffset))
    log(Debug(me + "Starting at bitOffset: " + bitOffset))

    var (endBitPosA : Long, state) = fillCharBufferMixedData(cb, bitOffset, decoder)
    var sb : StringBuilder = new StringBuilder // To keep track of the searched text
    val dSearch = new DelimSearcher with ConsoleLogger
    var buf = cb

    if (endBitPosA == -1L) {
      log(Debug(me + "Failed, reached end of buffer."))
      log(Debug("END_fillCharBufferUntilDelimiterOrEnd - ERR!"))
      //return (cb.toString(), -1L, SearchResult.NoMatch, null)
      return (null, -1L, SearchResult.EOD, null)
    }

    //println("START_CB: " + cb.toString())

    log(Debug(me + "Looking for: " + separators + " and terminators: " + terminators))

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
    var EOF : Boolean = false // Did we run off the end of the buffer?

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

  def fillCharBufferWithPatternMatch(cb : CharBuffer, bitOffset : Long, decoder : CharsetDecoder,
    pattern : String) : (String, Long, SearchResult) = {
    log(Debug("===\nSTART_FILL!\n===\n"))
    val byteOffsetAsLong = (bitOffset >> 3)
    val byteOffset = byteOffsetAsLong.toInt

    var (endBitPosA : Long, state) = fillCharBufferMixedData(cb, bitOffset, decoder)
    var sb : StringBuilder = new StringBuilder // To keep track of the searched text
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
    var EOF : Boolean = false

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

  def decodeNBytes(N : Int, array : Array[Byte], decoder : CharsetDecoder) : CharBuffer = {
    val list : Queue[Byte] = Queue.empty
    for (i <- 0 to N - 1) {
      list += array(i).toByte
      //      System.err.println(array(i).toByte.toHexString)
    }
    //    System.err.println
    val cb = decoder.decode(ByteBuffer.wrap(list.toArray[Byte]))
    cb
  }

  def decodeUntilFail(bytesArray: Array[Byte], decoder: CharsetDecoder, endByte: Long, maxCharBufLength: Int): (CharBuffer, Long) = {
    var cbFinal: CharBuffer = CharBuffer.allocate(1)
    var cbPrev: CharBuffer = CharBuffer.allocate(1)
    var numBytes: Int = 1

    while (numBytes <= endByte) {
      try {
        cbPrev = decodeNBytes(numBytes, bytesArray, decoder)
        if (cbPrev.length() > maxCharBufLength){ return (cbFinal, (numBytes -1))}
        cbFinal = cbPrev
      } catch {

        case e: Exception => //log(Debug("Exception in decodeUntilFail: " + e.toString()))

      }
      numBytes += 1
    }
    (cbFinal, (numBytes - 1))
  }

  // Fills the CharBuffer with as many bytes as can be decoded successfully.
  //
  def fillCharBufferMixedData(cb : CharBuffer, bitOffset : Long, decoder : CharsetDecoder, numBytes : Long = -1) : (Long, Boolean) = {
    withLoggingLevel(LogLevel.Info) {

      PECheck(bitOffset >= 0 && bitOffset % 8 == 0, "Characters must begin on byte boundaries. bitOffset = %s, alignment %s (bits)", bitOffset, bitOffset % 8)
      val byteOffsetAsLong = (bitOffset >> 3)
      PECheck(byteOffsetAsLong <= Int.MaxValue, "maximum offset (in bytes) cannot exceed Int.MaxValue")
      val byteOffset = byteOffsetAsLong.toInt
      val me : String = "fillCharBufferMixedData - "
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

      var byteArray : Array[Byte] = new Array[Byte](bb.limit() - byteOffset)

      // Retrieve a byte array from offset to end of ByteBuffer.
      // Starts at 0, because ByteBuffer was already set to byteOffset
      // Ends at ByteBuffer limit in Bytes minus the offset
      bb.get(byteArray, 0, (bb.limit - byteOffset))

      var endAtByte = numBytes

      if (endAtByte == -1) { endAtByte = bb.limit }

      log(Debug("endAtByte: %s", endAtByte))

      var (result : CharBuffer, bytesDecoded : Long) = decodeUntilFail(byteArray, decoder, endAtByte, cb.length())

      if (bytesDecoded == 0) { return (-1L, true) }

      log(Debug("MixedDataResult: BEG_" + result + "_END , bytesDecoded: " + bytesDecoded))

      cb.clear()
      cb.append(result)

      cb.flip() // so the caller can now read the sb.

      val endBytePos = byteOffset + bytesDecoded

      log(Debug(me + "Ended at byte pos " + endBytePos))

      var EOF : Boolean = bb.limit() == bb.position()

      bb.position(0) // prevent anyone depending on the buffer position across calls to any of the InStream methods.

      val endBitPos : Long = endBytePos << 3

      log(Debug(me + "Ended at bit pos " + endBitPos))

      (endBitPos, EOF)
    }
  }

  // Read the delimiter if possible off of the ByteBuffer
  //
  def getDelimiter(cb: CharBuffer, bitOffset: Long, 
      decoder: CharsetDecoder, separators: Set[String], terminators: Set[String],
      es: EscapeSchemeObj): (String, Long, Long, SearchResult, Delimiter) = {
    withLoggingLevel(LogLevel.Info) {

    log(Debug("BEG_getDelimiter"))

    val me : String = "getDelimiter - "

    log(Debug(me + "Looking for: " + separators + " AND " + terminators))

    val byteOffsetAsLong = (bitOffset >> 3)

    val byteOffset = byteOffsetAsLong.toInt

    log(Debug(me + "ByteOffset: " + byteOffset + " BitOffset: " + bitOffset))

    var (endBitPos : Long, state) = fillCharBufferMixedData(cb, bitOffset, decoder)
    var endBitPosA : Long = endBitPos

    if (endBitPos == -1L) {
      log(Debug(me + "Failed, reached end of buffer."))
      log(Debug("END_getDelimiter - End of Buffer!"))
      return (cb.toString(), -1L, -1L, SearchResult.NoMatch, null)
    }

    var sb : StringBuilder = new StringBuilder // To keep track of the searched text
    val dSearch = new DelimSearcher with Logging
    var buf = cb

    dSearch.setEscapeScheme(es)

    separators foreach { x => dSearch.addSeparator(x) }

    terminators foreach { x => dSearch.addTerminator(x) }

    var (theState, result, endPos, endPosDelim, theDelimiter) = dSearch.search(buf, 0)

    if (theDelimiter == null) { return (cb.toString(), -1L, -1L, SearchResult.NoMatch, null) }

    log(Debug("theDelimiter: " + theDelimiter.toString() + " theState: " + theState))

    //if (theDelimiter.typeDef == DelimiterType.Terminator) { return (cb.toString(), -1L, -1L, SearchResult.NoMatch, null) }

    if (theState == SearchResult.FullMatch) {
      sb.append(result)
    }

    var EOF : Boolean = false // Flag to indicate if we ran out of data to fill CharBuffer with

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
    
    // Fix for basicNest2 failing.  Need to acknowledge that we were terminated by an enclosing terminator 
    // but do not need to consume that terminator!  We should only consume OUR separators/terminators.
    //
    if (theDelimiter.typeDef == DelimiterType.Terminator) { 
      log(Debug(me + "Enclosing terminator found (" + theDelimiter.toString() + ")"))
      //return (sb.toString(), endPos, endPos, SearchResult.NoMatch, null) 
      return (sb.toString(), endPos, endPos, SearchResult.NoMatch, null) 
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
    var endBitPosDelimA : Long = endBitPosA

    if (endPosDelim != -1) {
      endBitPosDelimA = bitOffset + (resNumBytes * 8)
    }

    log(Debug(me + "Ended at BytePos: " + (byteOffset + resNumBytes)))
    log(Debug(me + "Ended at bitPos: " + endBitPosA))
    log(Debug("END_getDelimiter"))
    
    if (endPos != -1 && endPosDelim != -1){ (cb.subSequence(endPos, endPosDelim+1).toString(), endBitPosA, endBitPosDelimA, theState, theDelimiter) }

    else { (cb.toString(), endBitPosA, endBitPosDelimA, theState, theDelimiter) }
    }
  }
  
  // Read the delimiter if possible off of the ByteBuffer
  //
  def getDelimiterNilValue(cb: CharBuffer, bitOffset: Long, 
      decoder: CharsetDecoder, separators: Set[String], terminators: Set[String],
      es: EscapeSchemeObj): (String, Long, Long, SearchResult, Delimiter) = {
    withLoggingLevel(LogLevel.Info) {

    log(Debug("BEG_getDelimiterNilValue"))

    val me : String = "getDelimiterNilValue - "

    log(Debug(me + "Looking for: " + separators + " AND " + terminators))

    val byteOffsetAsLong = (bitOffset >> 3)

    val byteOffset = byteOffsetAsLong.toInt

    log(Debug(me + "ByteOffset: " + byteOffset + " BitOffset: " + bitOffset))

    var (endBitPos : Long, state) = fillCharBufferMixedData(cb, bitOffset, decoder)
    var endBitPosA : Long = endBitPos

    if (endBitPos == -1L) {
      log(Debug(me + "Failed, reached end of buffer."))
      log(Debug("END_getDelimiterNilValue - End of Buffer!"))
      return (cb.toString(), -1L, -1L, SearchResult.NoMatch, null)
    }

    var sb : StringBuilder = new StringBuilder // To keep track of the searched text
    val dSearch = new DelimSearcher with Logging
    var buf = cb

    dSearch.setEscapeScheme(es)

    separators foreach { x => dSearch.addSeparator(x) }

    terminators foreach { x => dSearch.addTerminator(x) }

    var (theState, result, endPos, endPosDelim, theDelimiter) = dSearch.search(buf, 0)

    if (theDelimiter == null) { return (cb.toString(), -1L, -1L, SearchResult.NoMatch, null) }

    log(Debug("theDelimiter: " + theDelimiter.toString() + " theState: " + theState))

    //if (theDelimiter.typeDef == DelimiterType.Terminator) { return (cb.toString(), -1L, -1L, SearchResult.NoMatch, null) }

    if (theState == SearchResult.FullMatch) {
      sb.append(result)
    }

    var EOF : Boolean = false // Flag to indicate if we ran out of data to fill CharBuffer with

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
    
    // For LiteralValueNil we do not care of this is an enclosing Terminator, we just
    // need to detect that we reached it.

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
    var endBitPosDelimA : Long = endBitPosA

    if (endPosDelim != -1) {
      endBitPosDelimA = bitOffset + (resNumBytes * 8)
    }

    log(Debug(me + "Ended at BytePos: " + (byteOffset + resNumBytes)))
    log(Debug(me + "Ended at bitPos: " + endBitPosA))
    log(Debug("END_getDelimiter"))
    
    if (endPos != -1 && endPosDelim != -1){ (cb.subSequence(endPos, endPosDelim+1).toString(), endBitPosA, endBitPosDelimA, theState, theDelimiter) }

    else { (cb.toString(), endBitPosA, endBitPosDelimA, theState, theDelimiter) }
    }
  }

  def getByte(bitPos : Long, order : java.nio.ByteOrder) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    bb.order(order)
    bb.get(bytePos) // NOT called getByte(pos)
  }
    
  def getShort(bitPos : Long, order : java.nio.ByteOrder) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    bb.order(order)
    bb.getShort(bytePos)
  }
    
  def getInt(bitPos : Long, order : java.nio.ByteOrder) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    bb.order(order)
    bb.getInt(bytePos)
  }
  
  def getLong(bitPos : Long, order : java.nio.ByteOrder) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    bb.order(order)
    bb.getInt(bytePos)
  }

  def getDouble(bitPos : Long, order : java.nio.ByteOrder) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    bb.order(order)
    val double = bb.getDouble(bytePos)
    double
  }

  def getFloat(bitPos : Long, order : java.nio.ByteOrder) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    bb.order(order)
    bb.getFloat(bytePos)
  }

  def getByteArray(bitPos : Long, order : java.nio.ByteOrder, size: Int) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    bb.order(order)
    bb.position(bytePos)
    var ret: Array[Byte] = new Array[Byte](size)
    bb.get(ret, 0, size)
    ret
  }
  
  def withLimit(startBitPos : Long, endBitPos : Long) = {
    Assert.invariant((startBitPos & 7) == 0)
    Assert.invariant((endBitPos & 7) == 0)
    val startByte = startBitPos / 8
    val endByte = (endBitPos + 7) / 8
    val count = endByte - startByte
    var bytes: Array[Byte] = new Array(count.asInstanceOf[Int])
    val oldPos = bb.position
    bb.position(startByte.asInstanceOf[Int])
    bb.get(bytes, 0, count.asInstanceOf[Int])
    val inputStream = new ByteArrayInputStream(bytes)
    val rbc = java.nio.channels.Channels.newChannel(inputStream)
    bb.position(oldPos)
    rbc
  }
}

