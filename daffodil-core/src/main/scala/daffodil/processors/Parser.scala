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
import scala.util.parsing.input.Reader
import delimsearch.DFDLCharReader
import scala.collection.mutable.HashMap
import java.util.UUID
import java.math.BigInteger

abstract class ProcessingError extends Exception with DiagnosticImplMixin

class ParseError(sc: SchemaComponent, val pstate: Option[PState], kind: String, args: Any*)
  extends ProcessingError {
  def isError = true
  def getSchemaLocations: Seq[SchemaLocation] = List(sc)
  def getDataLocations: Seq[DataLocation] = pstate.map { _.currentLocation }.toList

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
    val res = "Parse Error: " + msg +
      "\nContext was : %s".format(sc) +
      pstate.map { ps => "\nData location was preceding %s".format(ps.currentLocation) }.getOrElse("(no data location)")
    res
  }

  override def getMessage = toString
}

class AssertionFailed(sc: SchemaComponent, state: PState, msg: String)
  extends ParseError(sc, Some(state), "Assertion failed. %s", msg)

class ParseAlternativeFailed(sc: SchemaComponent, state: PState, val errors: Seq[Diagnostic])
  extends ParseError(sc, Some(state), "Alternative failed. Reason(s): %s", errors)

class AltParseFailed(sc: SchemaComponent, state: PState,
                     diags: Seq[Diagnostic])
  extends ParseError(sc, Some(state), "All alternatives failed. Reason(s): %s", diags) {

  override def getSchemaLocations: Seq[SchemaLocation] = diags.flatMap { _.getSchemaLocations }

  override def getDataLocations: Seq[DataLocation] = {
    // all should have the same starting location if they are alternatives.
    val dataLocs = diags.flatMap { _.getDataLocations }
    // TBD: what is the idiom for "insert a equals sign between all the elements of the list...??"
    // Well, this works, but isn't there a one-liner for this idiom.
    val allAreSame = dataLocs match {
      case f :: r => !r.exists { _ != f }
      case _ => true
    }
    Assert.invariant(allAreSame)
    diags.head.getDataLocations
  }
}

/**
 * Encapsulates lower-level parsing with a uniform interface
 */
abstract class Parser(val context: SchemaComponent) extends Logging {

  def PE(pstate: PState, s: String, args: Any*) = {
    pstate.failed(new ParseError(context, Some(pstate), s, args: _*))
  }

  def processingError(state: PState, str: String, args: Any*) =
    PE(state, str, args) // long form synonym

  protected def parse(pstate: PState): PState

  final def parse1(pstate: PState, context: SchemaComponent): PState = {
    Debugger.before(pstate, this)
    val afterState = parse(pstate)
    Debugger.after(pstate, afterState, this)
    afterState
  }

  // TODO: other methods for things like asking for the ending position of something
  // which would enable fixed-length formats to skip over data and not parse it at all.

  /**
   * BriefXML is XML-style output, but intended for specific purposes. It is NOT
   * an XML serialization of the data structure. It's an XML-style string, suitable to
   * manipulate, by people, in XML tooling. E.g., can stick into an XML editor to
   * then get it all indented nicely, use a structure editor to expand/collapse subregions,
   * but it is NOT intended to capture all of the state of the object.
   */
  def toBriefXML(depthLimit: Int = -1): String
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

  def context: SchemaComponent

  /**
   * Use to check for parse errors.
   *
   * Must be used only in the context of the withParseErrorThrowing wrapper.
   *
   * The schema component providing the context is implicit (via def context virtual member)
   */
  def PECheck(
    testTrueMeansOK: => Boolean,
    kind: String, args: Any*) {
    Assert.usage(WithParseErrorThrowing.flag, "Must use inside of withParseErrorThrowing construct.")
    if (!testTrueMeansOK) {
      throw new ParseError(context, None, kind, args: _*)
    }
  }

  /**
   * Passing the context explicitly
   */
  def PECheck(contextArg: SchemaComponent,
              testTrueMeansOK: => Boolean,
              kind: String, args: Any*) {
    Assert.usage(WithParseErrorThrowing.flag, "Must use inside of withParseErrorThrowing construct.")
    if (!testTrueMeansOK) {
      throw new ParseError(contextArg, None, kind, args: _*)
    }
  }

  def PE(kind: String, args: Any*): Nothing = {
    PE(context, kind, args: _*)
  }

  def PE(context: SchemaComponent, kind: String, args: Any*): Nothing = {
    Assert.usage(WithParseErrorThrowing.flag, "Must use inside of withParseErrorThrowing construct.")
    throw new ParseError(context, None, kind, args: _*)
  }

  /**
   * Wrap around parser code that wants to throw parse errors (e.g., parsers which call things which
   * call things which detect a parse error want to throw back to this)
   *
   * This wrapper then implements the required behavior for parsers
   * that being returning a failed parser state.
   */
  def withParseErrorThrowing(pstate: PState)(body: => PState): PState = {
    val saveCanThrowParseErrors = WithParseErrorThrowing.flag
    WithParseErrorThrowing.flag = true
    val result =
      try body
      catch {
        case e: ParseError => {
          val maybePS = e.pstate
          // if there is a maybePS, then use it to create the failed state (because it 
          // is probably more specific about the failure location), otherwise
          // use the one passed as an argument. 
          val res = maybePS.map { _.failed(e) }.getOrElse(pstate.failed(e))
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
      } finally {
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
  def SDECheck(testTrueMeansOK: => Boolean, context: SchemaComponent, pstate: PState, kind: String, args: Any*) = {
    if (!testTrueMeansOK) {
      throw new SchemaDefinitionError(Some(context), None, kind, args: _*)
    }
  }
}

/**
 * Global flag to insure we aren't throwing ParseErrors in a context that won't catch them
 * properly.
 */
object WithParseErrorThrowing {
  var flag: Boolean = false
}

// No-op, in case an optimization lets one of these sneak thru. 
// TODO: make this fail, and test optimizer sufficiently to know these 
// do NOT get through.
class EmptyGramParser(context: Term = null) extends Parser(context) {
  def parse(pstate: PState) = Assert.invariantFailed("EmptyGramParsers are all supposed to optimize out!")
  def toBriefXML(depthLimit: Int = -1) = "<empty/>"
  override def toString = toBriefXML()
}

class ErrorParser(context: Term = null) extends Parser(context) {
  def parse(pstate: PState): PState = Assert.abort("Error Parser")
  def toBriefXML(depthLimit: Int = -1) = "<error/>"
  override def toString = "Error Parser"
}

trait ToBriefXMLImpl {

  def nom: String
  def childParsers: Seq[Parser]

  // TODO: make this do indenting and newlines (maybe optionally?)
  def toBriefXML(depthLimit: Int = -1) = {
    if (depthLimit == 0) "..."
    else if (depthLimit == 1) "<seq>...</seq>"
    else {
      val lessDepth = depthLimit - 1
      "<" + nom + ">" + childParsers.map { _.toBriefXML(lessDepth) }.mkString + "</" + nom + ">"
    }
  }

  override def toString = toBriefXML() // pParser.toString + " ~ " + qParser.toString
}

class SeqCompParser(context: AnnotatedSchemaComponent, children: Seq[Gram])
  extends Parser(context)
  with ToBriefXMLImpl {
  Assert.invariant(!children.exists { _.isEmpty })

  val nom = "seq"

  val childParsers = children.map { _.parser }

  def parse(pstate: PState): PState = {
    var pResult = pstate
    childParsers.foreach { parser =>
      {
        pResult = parser.parse1(pResult, context)
        if (pResult.status != Success) {
          // failed in a sequence
          return pResult
        }
        pResult = pResult
      }
    }
    pResult
  }

}

class AltCompParser(context: AnnotatedSchemaComponent, children: Seq[Gram])
  extends Parser(context)
  with ToBriefXMLImpl {
  Assert.invariant(!children.exists { _.isEmpty })

  val nom = "alt"

  val childParsers = children.map { _.parser }

  def parse(pInitial: PState): PState = {
    val pStart = pInitial.withNewPointOfUncertainty
    var pResult: PState = null
    var diagnostics: Seq[Diagnostic] = Nil
    val cloneNode = pStart.captureJDOM // we must undo side-effects on the JDOM if we backtrack.
    childParsers.foreach { parser =>
      {
        log(Debug("Trying choice alternative: %s", parser))
        try {
          pResult = parser.parse1(pStart, context)
        } catch {
          case u: UnsuppressableException => throw u
          case e: Exception => Assert.invariantFailed("Runtime parsers should not throw exceptions: " + e)
        }
        if (pResult.status == Success) {
          log(Debug("Choice alternative success: %s", parser))
          val res = pResult.withRestoredPointOfUncertainty
          return res
        }
        // If we get here, then we had a failure
        log(Debug("Choice alternative failed: %s", parser))
        // Unwind any side effects on the Infoset 
        // The infoset is the primary non-functional data structure. We have to un-side-effect it.
        pStart.restoreJDOM(cloneNode)
        val diag = new ParseAlternativeFailed(context, pStart, pResult.diagnostics)
        diagnostics = diag +: diagnostics
        // check for discriminator evaluated to true.
        if (pResult.discriminator == true) {
          log(Debug("Failure, but discriminator true. Additional alternatives discarded."))
          // If so, then we don't run the next alternative, we
          // consume this discriminator status result (so it doesn't ripple upward)
          // and return the failed state withall the diagnostics.
          //
          val allDiags = new AltParseFailed(context, pResult, diagnostics.reverse)
          val res = pResult.failed(allDiags).withRestoredPointOfUncertainty
          return res
        }
        //
        // Here we have a failure, but no discriminator was set, so we try the next alternative.
        // Which means we just go around the loop
      }
    }
    // Out of alternatives. All of them failed. 
    val allDiags = new AltParseFailed(context, pStart, diagnostics.reverse)
    val allFailedResult = pStart.failed(allDiags)
    log(Debug("All AltParser alternatives failed."))
    val result = allFailedResult.withRestoredPointOfUncertainty
    result
  }

}

case class DummyParser(sc: PropertyMixin) extends Parser(null) {
  def parse(pstate: PState): PState = Assert.abort("Parser for " + sc + " is not yet implemented.")

  def toBriefXML(depthLimit: Int = -1) = "<dummy/>"
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
    var bytes: List[Byte] = Nil
    try {
      for (i <- 0 to 40) {
        bytes = inStream.getByte(aligned64BitsPos + (i * 8), java.nio.ByteOrder.BIG_ENDIAN) +: bytes
      }
    } catch {
      case e: IndexOutOfBoundsException =>
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
    val chars = for { i <- 0 to count - 1 } yield arr(i)
    chars.mkString("")
  }

  def dump = {
    bytes2Hex(byteDump)
  }

  /*
   * We're at the end if an attempt to get a bit fails with an index exception
   */
  def isAtEnd: Boolean = {
    try {
      inStream.getBitSequence(bitPos, 1, java.nio.ByteOrder.BIG_ENDIAN)
      false
    } catch {
      case e: IndexOutOfBoundsException => {
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
class PStateStream(val inStream: InStream, val bitLimit: Long, val charLimit: Long = -1,
                   val bitPos: Long = 0, val charPos: Long = 0, val reader: Option[DFDLCharReader],
                   val contextMap: HashMap[UUID, ElementBase] = HashMap.empty) {
  Assert.invariant(bitPos >= 0)
  def withInStream(inStream: InStream, status: ProcessorResult = Success) =
    new PStateStream(inStream, bitLimit, charLimit, bitPos, charPos, reader, contextMap)
  def withPos(bitPos: Long, charPos: Long, status: ProcessorResult = Success) =
    new PStateStream(inStream, bitLimit, charLimit, bitPos, charPos, reader, contextMap)
  def withEndBitLimit(bitLimit: Long, status: ProcessorResult = Success) =
    new PStateStream(inStream, bitLimit, charLimit, bitPos, charPos, reader, contextMap)
}
object PStateStream {
  def initialPStateStream(in: InStream, bitOffset: Long) =
    new PStateStream(in, bitLimit = -1, bitPos = bitOffset, reader = None, contextMap = HashMap.empty)
}

/**
 * A parser takes a state, and returns an updated state
 *
 * The fact that there are side-effects/mutations on parts of the state
 * enables us to reuse low-level java primitives that mutate streams.
 *
 * The goal however, is to hide that fact so that the only places that have to
 * know are the places doing the mutation, and the places rolling them back
 * which should be isolated to the alternative parser, and repParsers, i.e.,
 * places where points-of-uncertainty are handled.
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
  val occursCountStack: List[Long],
  val diagnostics: List[Diagnostic],
  val discriminatorStack: List[Boolean]) extends DFDL.State {
  def bytePos = bitPos >> 3
  def whichBit = bitPos % 8
  def groupPos = if (groupIndexStack != Nil) groupIndexStack.head else -1
  def childPos = if (childIndexStack != Nil) childIndexStack.head else -1
  def arrayPos = if (arrayIndexStack != Nil) arrayIndexStack.head else -1
  def occursCount = occursCountStack.head

  override def toString() = {
    "PState( bitPos=%s charPos=%s success=%s contextMapCount=%s )".format(bitPos, charPos, status, contextMapCount)
  }
  def discriminator = discriminatorStack.head
  def currentLocation: DataLocation = new DataLoc(bitPos, inStream)
  def inStreamState = inStreamStateStack top
  def inStream = inStreamState inStream
  def bitPos = inStreamState bitPos
  def bitLimit = inStream.asInstanceOf[InStreamFromByteChannel].byteReader.bb.limit() * 8 //inStreamState bitLimit
  def charPos = inStreamState charPos
  def charLimit = inStreamState charLimit
  def parentElement = parent.asInstanceOf[Element]
  def parentForAddContent =
    parent.asInstanceOf[{
      def addContent(c: org.jdom.Content): Unit
      def removeContent(c: org.jdom.Content): Unit
    }]
  def textReader = inStreamState reader
  def contextMap = inStreamState contextMap
  def contextMapCount = contextMap.size
  def getContextByUID(uid: String): Option[ElementBase] = {
    val ctxMap = inStreamState.contextMap
    try {
      val uuid = UUID.fromString(uid)
      return ctxMap.get(uuid)
    } catch {
      case u: UnsuppressableException => throw u
      case e: Exception => return None
    }
    None
  }
  def addContext(sc: ElementBase): UUID = {
    val ctxMap = inStreamState.contextMap
    val uuid = UUID.randomUUID()
    ctxMap.put(uuid, sc)
    uuid
  }

  /**
   * Convenience functions for creating a new state, changing only
   * one or a related subset of the state components to a new one.
   */

  def withInStreamState(inStreamState: PStateStream, status: ProcessorResult = Success) =
    new PState(inStreamStateStack push (inStreamState), parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminatorStack)
  def withInStream(inStream: InStream, status: ProcessorResult = Success) =
    new PState(inStreamStateStack push (new PStateStream(inStream, bitLimit, charLimit, bitPos, charPos, textReader, contextMap)), parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminatorStack)
  def withLastInStream(status: ProcessorResult = Success) = {
    var lastBitPos = bitPos
    var lastCharPos = if (charPos > 0) charPos else 0
    inStreamStateStack pop ()
    new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminatorStack) withPos (bitPos + lastBitPos, charPos + lastCharPos)
  }
  def withPos(bitPos: Long, charPos: Long, status: ProcessorResult = Success) = {
    var newInStreamStateStack = inStreamStateStack clone ()
    newInStreamStateStack pop ()
    newInStreamStateStack push (new PStateStream(inStream, bitLimit, charLimit, bitPos, charPos, None, contextMap))
    new PState(newInStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminatorStack)
  }
  def withEndBitLimit(bitLimit: Long, status: ProcessorResult = Success) = {
    var newInStreamStateStack = inStreamStateStack clone ()
    newInStreamStateStack pop ()
    newInStreamStateStack push (new PStateStream(inStream, bitLimit, charLimit, bitPos, charPos, textReader, contextMap))
    new PState(newInStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminatorStack)
  }
  def withParent(parent: org.jdom.Parent, status: ProcessorResult = Success) =
    new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminatorStack)
  def withVariables(variableMap: VariableMap, status: ProcessorResult = Success) =
    new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminatorStack)
  def withGroupIndexStack(groupIndexStack: List[Long], status: ProcessorResult = Success) =
    new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminatorStack)
  def withChildIndexStack(childIndexStack: List[Long], status: ProcessorResult = Success) =
    new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminatorStack)
  def withArrayIndexStack(arrayIndexStack: List[Long], status: ProcessorResult = Success) =
    new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminatorStack)
  def setOccursCount(oc: Long) =
    new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, oc :: occursCountStack.tail, diagnostics, discriminatorStack)
  def withOccursCountStack(ocs: List[Long]) =
    new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, ocs, diagnostics, discriminatorStack)
  def failed(msg: => String): PState =
    failed(new GeneralParseFailure(msg))
  def failed(failureDiagnostic: Diagnostic) =
    new PState(inStreamStateStack, parent, variableMap, target, namespaces, new Failure(failureDiagnostic.getMessage), groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, failureDiagnostic :: diagnostics, discriminatorStack)

  def withNewPointOfUncertainty =
    new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, false +: discriminatorStack)
  def withRestoredPointOfUncertainty =
    new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminatorStack.tail)

  def withDiscriminator(disc: Boolean) =
    new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, disc +: discriminatorStack.tail)
  def withReader(newReader: Option[DFDLCharReader]) =
    new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminatorStack)
  def withReaderPos(bitPos: Long, charPos: Long, reader: DFDLCharReader, status: ProcessorResult = Success) = {
    var newInStreamStateStack = inStreamStateStack clone ()
    newInStreamStateStack pop ()
    //val newReader = reader.atPos(charPos.toInt)
    val newReader = reader.atBytePos((bitPos >> 3).toInt)
    newInStreamStateStack push (new PStateStream(inStream, bitLimit, charLimit, bitPos, charPos, Some(newReader), contextMap))
    new PState(newInStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminatorStack)
  }

  // Need last state for Assertion Pattern
  def withLastState = {
    var newInStreamStateStack = inStreamStateStack clone ()
    newInStreamStateStack pop ()
    new PState(inStreamStateStack, parent, variableMap, target, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, discriminatorStack)
  }

  /**
   * advance our position, as a child element of a parent, and our index within the current sequence group.
   *
   * These can be different because an element can have sequences nested directly in sequences. Those effectively all
   * get flattened into children of the element. The start of a sequence doesn't start the numbering of children. It's
   * the start of a complex type that does that.
   */
  def moveOverByOneElement = {
    val s1 = moveOverOneGroupIndexOnly
    val s2 = s1.moveOverOneElementChildOnly
    // val s3 = s2.moveOverOneArrayIndexOnly // move over in array happens only in the RepParsers
    s2
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

  def moveOverOneGroupIndexOnly = {
    groupIndexStack match {
      case Nil => this
      case hd :: tl => {
        val newGroupIndex = hd + 1
        withGroupIndexStack(newGroupIndex :: tl)
      }
    }
  }

  def moveOverOneArrayIndexOnly = {
    arrayIndexStack match {
      case Nil => this
      case hd :: tl => {
        val newArrayIndex = hd + 1
        withArrayIndexStack(newArrayIndex :: tl)
      }
    }
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
  def createInitialState(rootElemDecl: GlobalElementDecl, in: InStream, bitOffset: Long): PState = {
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
    val textReader: Option[DFDLCharReader] = None
    val newState = new PState(Stack(initPState), doc, variables, targetNamespace, namespaces, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, List(false))
    newState
  }

  /**
   * For testing it is convenient to just hand it strings for data.
   */
  def createInitialState(rootElemDecl: GlobalElementDecl, data: String, bitOffset: Long): PState = {
    val in = Compiler.stringToReadableByteChannel(data)
    createInitialState(rootElemDecl, in, data.length, bitOffset)
  }

  /**
   * Construct our InStream object and initialize the state block.
   */
  def createInitialState(rootElemDecl: GlobalElementDecl, input: DFDL.Input, sizeHint: Long = -1, bitOffset: Long = 0): PState = {
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

  def getByte(bitPos: Long, order: java.nio.ByteOrder): Byte
  def getShort(bitPos: Long, order: java.nio.ByteOrder): Short
  def getInt(bitPos: Long, order: java.nio.ByteOrder): Int
  def getLong(bitPos: Long, order: java.nio.ByteOrder): Long

  def getDouble(bitPos: Long, order: java.nio.ByteOrder): Double
  def getFloat(bitPos: Long, order: java.nio.ByteOrder): Float

  def getByteArray(bitPos: Long, order: java.nio.ByteOrder, size: Int): Array[Byte]
  def getBitSequence(bitPos: Long, bitCount: Long, order: java.nio.ByteOrder): (BigInt, Long)

  // def fillCharBufferUntilDelimiterOrEnd
}

class InStreamFromByteChannel(val context: ElementBase, in: DFDL.Input, sizeHint: Long = 1024 * 128)
  extends InStream
  with Logging
  with WithParseErrorThrowing {
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

  abstract class EndianTraits(val startBit: Long, val bitCount: Long) {
    lazy val byteLength = 8.toLong
    lazy val alignmentOffsetLength = startBit & 7
    lazy val isAligned = alignmentOffsetLength == 0
    lazy val startBitInByteRemaining = if (isAligned) byteLength - alignmentOffsetLength else 0
    lazy val shortByteLength = bitCount & 7
    lazy val wholeBytesLength = bitCount - shortByteLength
    lazy val wholeBytesSize = wholeBytesLength >>> 3
    lazy val isShortSplit = alignmentOffsetLength + shortByteLength > byteLength
    lazy val restOfBytesAlignment = (alignmentOffsetLength + initialByteLength) & 7
    lazy val finalBytesAlignment = (alignmentOffsetLength + shortByteLength) & 7
    lazy val isSplit = restOfBytesAlignment != 0
    lazy val longByteLength = if (wholeBytesSize == 0) 0 else byteLength
    val isInitialSplit: Boolean
    val isFinalSplit: Boolean
    val initialShiftLeft: Long
    val nextByteShiftLeft: Long
    val initialByteLength: Long
    val finalByteLength: Long
    lazy val initialTopByteShiftCount = if (isInitialSplit) restOfBytesAlignment else 0
    lazy val topByteShiftCount = restOfBytesAlignment
    lazy val finalTopByteShiftCount = if (isFinalSplit) finalBytesAlignment else 0
    lazy val initialTopByteLength = initialByteLength - initialTopByteShiftCount
    lazy val topByteLength = byteLength - restOfBytesAlignment
    lazy val finalTopByteLength = finalByteLength - finalTopByteShiftCount
    lazy val initialBottomByteLength = initialTopByteShiftCount
    lazy val bottomByteLength = topByteShiftCount
    lazy val finalBottomByteLength = finalTopByteShiftCount
    lazy val hasInitialByte = initialByteLength != 0
    lazy val hasFinalByte = finalByteLength != 0
    lazy val hasShortByte = shortByteLength != 0
  }
  case class BigEndianTraits(override val startBit: Long, override val bitCount: Long) extends EndianTraits(startBit, bitCount) {
    lazy val isInitialSplit = isShortSplit
    lazy val isFinalSplit = isSplit
    lazy val initialShiftLeft = if (hasShortByte) wholeBytesLength else wholeBytesLength - byteLength
    lazy val nextByteShiftLeft = -byteLength
    lazy val initialByteLength = shortByteLength
    lazy val finalByteLength = longByteLength
  }
  case class LittleEndianTraits(override val startBit: Long, override val bitCount: Long) extends EndianTraits(startBit, bitCount) {
    lazy val isInitialSplit = isSplit
    lazy val isFinalSplit = isShortSplit
    lazy val initialShiftLeft = 0.toLong
    lazy val nextByteShiftLeft = byteLength
    lazy val initialByteLength = longByteLength
    lazy val finalByteLength = shortByteLength
  }

  def getEndianTraits(bitPos: Long, bitCount: Long, order: java.nio.ByteOrder) = order match {
    case java.nio.ByteOrder.BIG_ENDIAN => BigEndianTraits(bitPos, bitCount)
    case java.nio.ByteOrder.LITTLE_ENDIAN => LittleEndianTraits(bitPos, bitCount)
    case _ => Assert.invariantFailed("Invalid Byte Order: " + order)
  }

  def getBitSequence(bitPos: Long, bitCount: Long, order: java.nio.ByteOrder): (BigInt, Long) = {
    val worker: EndianTraits = getEndianTraits(bitPos, bitCount, order)
    var result = BigInt(0)
    var position = worker.startBit
    var outShift = worker.initialShiftLeft

    // Read first byte (be it complete or partial)
    if (worker.hasInitialByte) {
      result =
        (BigInt(
          if (worker.isInitialSplit) {
            (getPartialByte(position, worker.initialTopByteLength, worker.initialTopByteShiftCount) |
              getPartialByte(position + worker.initialTopByteLength, worker.initialBottomByteLength, 0)).toByte
          } else {
            getPartialByte(position, worker.initialByteLength, 0)
          }) & 0xFF) << outShift.toInt
      position = position + worker.initialByteLength
      outShift = outShift + worker.nextByteShiftLeft
    }

    // Next all the middle bytes; we skip one byte because that will be handled either in the initial or final handler
    for (thisByte <- 1 until worker.wholeBytesSize.toInt) {
      result = result +
        ((BigInt(
          if (worker.isSplit) {
            (getPartialByte(position, worker.topByteLength, worker.topByteShiftCount) |
              getPartialByte(position + worker.topByteLength, worker.bottomByteLength, 0)).toByte
          } else {
            getPartialByte(position, worker.byteLength, 0)
          }) & 0xFF) << outShift.toInt)
      position = position + worker.byteLength
      outShift = outShift + worker.nextByteShiftLeft
    }

    // Read first byte (be it complete or partial)
    if (worker.hasFinalByte) {
      result = result +
        ((BigInt(
          if (worker.isFinalSplit) {
            (getPartialByte(position, worker.finalTopByteLength, worker.finalTopByteShiftCount) |
              getPartialByte(position + worker.finalTopByteLength, worker.finalBottomByteLength, 0)).toByte
          } else {
            getPartialByte(position, worker.finalByteLength, 0)
          }) & 0xFF) << outShift.toInt)
      position = position + worker.finalByteLength
      outShift = outShift + worker.nextByteShiftLeft
    }

    (result, position)
  }

  // littleEndian shift left except last, bigEndian shift right except first
  def getPartialByte(bitPos: Long, bitCount: Long, shift: Long = 0): Byte = {
    Assert.invariant(shift >= 0 && shift + bitCount <= 8)
    val bytePos = (bitPos >>> 3).toInt
    val bitOffset = (bitPos % 8).toByte
    var result = byteReader.bb.get(bytePos)

    if (bitCount != 8) {
      Assert.invariant(0 < bitCount && bitCount <= 8 && bitOffset + bitCount <= 8)
      val mask = ((1 << bitCount) - 1) << (8 - bitOffset - bitCount)

      result = (result & mask).toByte

      // Shift so LSB of result is at LSB of octet then mask off top bits
      val finalShift = 8 - bitCount - bitOffset - shift
      (if (finalShift < 0) result << -finalShift else result >> finalShift).toByte
    } else {
      // Verify byte alignment and disallow shift
      Assert.invariant(bitOffset == 0 && shift == 0)
      result
    }
  }

  def getByte(bitPos: Long, order: java.nio.ByteOrder) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    byteReader.bb.order(order)
    byteReader.bb.get(bytePos) // NOT called getByte(pos)
  }

  def getShort(bitPos: Long, order: java.nio.ByteOrder) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    byteReader.bb.order(order)
    val res = byteReader.bb.getShort(bytePos)
    //val res = (byteReader.bb.getShort(bytePos) & 0xffff).asInstanceOf[Short]
    res
  }

  def getInt(bitPos: Long, order: java.nio.ByteOrder) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    byteReader.bb.order(order)
    byteReader.bb.getInt(bytePos)
  }

  def getLong(bitPos: Long, order: java.nio.ByteOrder) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    byteReader.bb.order(order)
    val res = byteReader.bb.getLong(bytePos)
    res
  }

  def getDouble(bitPos: Long, order: java.nio.ByteOrder) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    byteReader.bb.order(order)
    val double = byteReader.bb.getDouble(bytePos)
    double
  }

  def getFloat(bitPos: Long, order: java.nio.ByteOrder) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    byteReader.bb.order(order)
    byteReader.bb.getFloat(bytePos)
  }

  def getByteArray(bitPos: Long, order: java.nio.ByteOrder, size: Int) = {
    Assert.invariant(bitPos % 8 == 0)
    val bytePos = (bitPos >> 3).toInt
    byteReader.bb.order(order)
    byteReader.bb.position(bytePos)
    var ret: Array[Byte] = new Array[Byte](size)
    byteReader.bb.get(ret, 0, size)
    ret
  }

  def withLimit(startBitPos: Long, endBitPos: Long) = {
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

