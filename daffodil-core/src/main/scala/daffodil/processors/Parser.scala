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
import scala.collection.JavaConversions._
import scala.util.logging.ConsoleLogger
import stringsearch.DelimSearcherV3._
import scala.collection.mutable.Queue
import scala.util.matching.Regex
import daffodil.util._
import daffodil.exceptions.ThrowsSDE
import java.io.ByteArrayInputStream
import scala.collection.mutable.Stack
import daffodil.debugger.Debugger
import daffodil.util.Misc._
import java.io.InputStreamReader
import java.io.BufferedReader
import daffodil.exceptions.UnsuppressableException

abstract class ProcessingError extends Exception with DiagnosticImplMixin

class ParseError(sc : SchemaComponent, val pstate : Option[PState], kind : String, args : Any*) 
extends ProcessingError {
  def isError = true
  def getSchemaLocations : Seq[SchemaLocation] = List(sc)
  def getDataLocations : Seq[DataLocation] = pstate.map { _.currentLocation }.toList
  
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


class ParseAlternativeFailed(sc : SchemaComponent, state : PState, val errors : Seq[Diagnostic]) 
extends ParseError(sc, Some(state), "Alternative failed. Reason(s): %s", errors) 

class AltParseFailed(sc : SchemaComponent, state : PState,
  val p : Diagnostic, val q : Diagnostic) 
  extends ParseError(sc, Some(state), "All alternatives failed. Reason(s): %s", List(p, q)) {
  
  override def getSchemaLocations : Seq[SchemaLocation] = p.getSchemaLocations ++ q.getSchemaLocations
  
  override def getDataLocations : Seq[DataLocation] = {
    // both should have the same starting location if they are alternatives.
    Assert.invariant(p.getDataLocations == q.getDataLocations)
    p.getDataLocations
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
        // TODO: Runtime SDEs should be distinguished somehow usefully.
//        case e : SchemaDefinitionError => {
//          val res = pstate.failed(e)
//          res
//        }
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
        case u :UnsuppressableException => throw u
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
        case u :UnsuppressableException => throw u
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

        val pAltErr = new ParseAlternativeFailed(context, pstate, pResult.diagnostics)
        val qAltErr = new ParseAlternativeFailed(context, pstate, qResult.diagnostics)
        val altErr = new AltParseFailed(context, pstate, pAltErr, qAltErr)

        val bothFailedResult = pstate.failed(altErr)
        log(Debug("Both AltParser alternatives failed."))

        val result = bothFailedResult
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
  
  override def toString() = "Location is byte " + bitPos / 8 + 
  "\nUTF-8 text starting at byte " + aligned64BitsPos / 8 + " is: (" + utf8Dump + ")" + 
  "\nData (hex) starting at byte " + aligned64BitsPos / 8 + " is: (" + dump + ")" 
  
  
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


//  def fillCharBuffer(buf: CharBuffer, bitOffset: Long, decoder: CharsetDecoder): Long
//  def fillCharBufferMixedData(cb: CharBuffer, bitOffset: Long, decoder: CharsetDecoder, endByte: Long = -1): (Long, Boolean)

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
{ 
  var byteReader: delimsearch.DFDLByteReader = new delimsearch.DFDLByteReader(in)
  
  def getBytes(bitPos: Long, numBytes: Int): Array[Byte] = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    val bb = byteReader.bb
    bb.position(bytePos)
    val result: Array[Byte] = new Array[Byte](numBytes)
    bb.get(result, 0, numBytes)
    result
  }
  
  def getBytesRemaining(bitPos: Long): Array[Byte] = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    val bb = byteReader.bb
    bb.position(bytePos)
    val numBytesRemaining = bb.remaining()
    val result: Array[Byte] = new Array[Byte](numBytesRemaining)
    bb.get(result, 0, numBytesRemaining)
    result
  }

  def getPartialByte(bitPos : Long, bitCount : Long) = {
    val bytePos = (bitPos >> 3).toInt
    var result = bb.get(bytePos)

    if (bitCount != 8) {
      val bitOffset = (bitPos % 8).toByte
      Assert.invariant(0 < bitCount && bitCount <= 8 && bitOffset + bitCount <= 8)

      // Shift so LSB of result is at LSB of octet then mask off top bits
      (result >> (8 - bitCount - bitOffset).asInstanceOf[Byte]) & ((1 << bitCount) - 1)
    }
    else {
      result
    }
  }

  def getByte(bitPos : Long, order : java.nio.ByteOrder) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    byteReader.bb.order(order)
    byteReader.bb.get(bytePos) // NOT called getByte(pos)
  }
    
  def getShort(bitPos : Long, order : java.nio.ByteOrder) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    byteReader.bb.order(order)
    byteReader.bb.getShort(bytePos)
  }
    
  def getInt(bitPos : Long, order : java.nio.ByteOrder) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    byteReader.bb.order(order)
    byteReader.bb.getInt(bytePos)
  }
  
  def getLong(bitPos : Long, order : java.nio.ByteOrder) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    byteReader.bb.order(order)
    byteReader.bb.getInt(bytePos)
  }

  def getDouble(bitPos : Long, order : java.nio.ByteOrder) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    byteReader.bb.order(order)
    val double = byteReader.bb.getDouble(bytePos)
    double
  }

  def getFloat(bitPos : Long, order : java.nio.ByteOrder) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    byteReader.bb.order(order)
   byteReader.bb.getFloat(bytePos)
  }

  def getByteArray(bitPos : Long, order : java.nio.ByteOrder, size: Int) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    byteReader.bb.order(order)
    byteReader.bb.position(bytePos)
    var ret: Array[Byte] = new Array[Byte](size)
    byteReader.bb.get(ret, 0, size)
    ret
  }
  
  def withLimit(startBitPos : Long, endBitPos : Long) = {
    // Appears to only be called from lengthKind=Pattern match code
    Assert.invariant((startBitPos & 7) == 0)
    Assert.invariant((endBitPos & 7) == 0)
    val startByte = startBitPos / 8
    val endByte = (endBitPos + 7) / 8
    val count = endByte - startByte
    var bytes: Array[Byte] = new Array(count.asInstanceOf[Int])
    val oldPos = byteReader.bb.position
    byteReader.bb.position(startByte.asInstanceOf[Int])
    byteReader.bb.get(bytes, 0, count.asInstanceOf[Int])
    val inputStream = new ByteArrayInputStream(bytes)
    val rbc = java.nio.channels.Channels.newChannel(inputStream)
    byteReader.bb.position(oldPos)
    rbc
  }
}

