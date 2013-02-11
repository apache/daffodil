package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.api._
import java.nio._
import java.nio.charset._
import scala.collection.JavaConversions._
import scala.util.logging.ConsoleLogger
import scala.util.matching.Regex
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import java.io.ByteArrayInputStream
import scala.collection.immutable.Stack
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.util.Misc._
import java.io.InputStreamReader
import java.io.BufferedReader
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import scala.util.parsing.input.Reader
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
      "\nSchema context: %s %s".format(sc, sc.locationDescription) +
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
 * @example {{{
 * withParseErrorThrowing(pstate) { // something enclosing like the parser
 * ...
 *   // calls something which calls something which eventually calls
 *       PECheck(bitOffset % 8 == 0, "must be byte boundary, not bit %s", bitOffset)
 * ...
 * }
 * }}}
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
    val cloneNode = pStart.captureInfosetElementState // we must undo side-effects on the JDOM if we backtrack.
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
        pStart.restoreInfosetElementState(cloneNode)
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

case class DummyParser(sc: SchemaComponent) extends Parser(null) {
  def parse(pstate: PState): PState = sc.SDE("Parser for " + sc + " is not yet implemented.")

  def toBriefXML(depthLimit: Int = -1) = "<dummy/>"
  override def toString = if (sc == null) "Dummy[null]" else "Dummy[" + sc + "]"
}

class GeneralParseFailure(msg: String) extends Throwable with DiagnosticImplMixin {
  Assert.usage(msg != null && msg != "")
  def isError() = true
  def getSchemaLocations() = Nil
  def getDataLocations() = Nil
  override def getMessage() = msg
}

/**
 * A placeholder for holding the complete stream information so that it can be popped all at once when a new stream
 * state is created
 *
 * @param inStream Input Data Stream
 * @param bitLimit Total Bits available in given Data Stream
 * @param charLimit Total UNICODE or given Character Set Characters in given Data Stream
 * @param bitPos Current Read Position in given Data Stream
 * @param charPos Current Read Character Position (in decoded characters, i.e., does not correspond to bytes in any way) for the given Data Stream
 */
//case class PStateStream private (val inStream: InStream, val bitLimit: Long, val charLimit: Long = -1,
//                                 val bitPos: Long = 0, val charPos: Long = 0, val reader: Option[DFDLCharReader]) {
//  Assert.invariant(bitPos >= 0)
//  def withInStream(newInStream: InStream) = copy(inStream = newInStream)
//  def withPos(newBitPos: Long, newCharPos: Long, newReader: Option[DFDLCharReader]) = copy(bitPos = newBitPos, charPos = newCharPos, reader = newReader)
//  def withEndBitLimit(newBitLimit: Long) = copy(bitLimit = newBitLimit)
//}
//
//object PStateStream {
//  def initialPStateStream(in: InStream, bitOffset: Long, bitLimit: Long) =
//    PStateStream(in, bitLimit, bitPos = bitOffset, reader = None)
//}

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
case class PState(
  val inStream: InStream,
  val infoset: InfosetItem,
  val variableMap: VariableMap,
  val target: NS,
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
    "PState( bitPos=%s charPos=%s status=%s )".format(bitPos, charPos, status)
  }
  def discriminator = discriminatorStack.head
  def currentLocation: DataLocation = new DataLoc(bitPos, bitLimit, inStream)
  // def inStreamState = inStreamStateStack top
  def bitPos = inStream bitPos
  def bitLimit = inStream bitLimit
  def charPos = inStream charPos
  def charLimit = inStream charLimit
  def parentElement = infoset.asInstanceOf[InfosetElement]
  def parentDocument = infoset.asInstanceOf[InfosetDocument]
  def textReader = inStream reader

  /**
   * Convenience functions for creating a new state, changing only
   * one or a related subset of the state components to a new one.
   */

  //  def withInStreamState(newInStreamState: PStateStream, newStatus: ProcessorResult = Success) =
  //    copy(inStreamState = newInStreamState, status = newStatus)
  //
  //  def withInStream(newInStream: InStream, newStatus: ProcessorResult = Success) = {
  //    val newInStreamState = inStreamState.withInStream(newInStream)
  //    copy(inStreamState = newInStreamState, status = newStatus)
  //  }

  //  def withLastInStream(newStatus: ProcessorResult = Success) = {
  //    val lastBitPos = bitPos
  //    val lastCharPos = if (charPos > 0) charPos else 0
  //    val cp = copy(inStreamStateStack = inStreamStateStack pop, status = newStatus)
  //    val res = cp.withPos(bitPos + lastBitPos, charPos + lastCharPos)
  //    res
  //  }

  def withPos(bitPos: Long, charPos: Long, newStatus: ProcessorResult = Success) = {
    val newInStream = inStream.withPos(bitPos, charPos, None)
    copy(inStream = newInStream, status = newStatus)
  }

  def withEndBitLimit(bitLimit: Long, newStatus: ProcessorResult = Success) = {
    var newInStream = inStream.withEndBitLimit(bitLimit)
    copy(inStream = newInStream, status = newStatus)
  }

  def withParent(newParent: InfosetItem, newStatus: ProcessorResult = Success) =
    copy(infoset = newParent, status = newStatus)
  def withVariables(newVariableMap: VariableMap, newStatus: ProcessorResult = Success) =
    copy(variableMap = newVariableMap, status = newStatus)
  def withGroupIndexStack(newGroupIndexStack: List[Long], newStatus: ProcessorResult = Success) =
    copy(groupIndexStack = newGroupIndexStack, status = newStatus)
  def withChildIndexStack(newChildIndexStack: List[Long], newStatus: ProcessorResult = Success) =
    copy(childIndexStack = newChildIndexStack, status = newStatus)
  def withArrayIndexStack(newArrayIndexStack: List[Long], newStatus: ProcessorResult = Success) =
    copy(arrayIndexStack = newArrayIndexStack, status = newStatus)
  def setOccursCount(oc: Long) =
    copy(occursCountStack = oc :: occursCountStack.tail)
  def withOccursCountStack(ocs: List[Long]) =
    copy(occursCountStack = ocs)

  def failed(msg: => String): PState =
    failed(new GeneralParseFailure(msg))

  def failed(failureDiagnostic: Diagnostic) = {
    copy(status = new Failure(failureDiagnostic.getMessage),
      diagnostics = failureDiagnostic :: diagnostics)
  }

  def withNewPointOfUncertainty = {
    copy(discriminatorStack = false +: discriminatorStack)
  }

  def withRestoredPointOfUncertainty =
    copy(discriminatorStack = discriminatorStack.tail)

  def withDiscriminator(disc: Boolean) =
    copy(discriminatorStack = disc +: discriminatorStack.tail)

  // TODO: REMOVE Has to be broken. Doesn't do anything.
  //def withReader(newReader: Option[DFDLCharReader]) =
  //    copy()

  def withReaderPos(bitPos: Long, charPos: Long, reader: DFDLCharReader, status: ProcessorResult = Success) = {
    val newReader = reader.atBitPos(bitPos)
    val newInStream = inStream.withPos(bitPos, charPos, Some(newReader))
    copy(inStream = newInStream)
  }

  // Need last state for Assertion Pattern
  // def withLastState = copy(inStreamStateStack = inStreamStateStack.pop)

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

  def captureInfosetElementState = parentElement.captureState()

  def restoreInfosetElementState(st: Infoset.ElementState) = parentElement.restoreState(st)

}

object PState {

  /**
   * Initialize the state block given our InStream and a root element declaration.
   */
  def createInitialState(
    rootElemDecl: GlobalElementDecl,
    in: InStream): PState = {

    val doc = Infoset.newDocument()
    val variables = rootElemDecl.schemaDocument.schemaSet.variableMap
    val targetNamespace = rootElemDecl.schemaDocument.targetNamespace
    val status = Success
    val groupIndexStack = Nil
    val childIndexStack = Nil
    val arrayIndexStack = Nil
    val occursCountStack = Nil
    val diagnostics = Nil
    val discriminator = false
    val textReader: Option[DFDLCharReader] = None
    val newState = PState(in, doc, variables, targetNamespace, status, groupIndexStack, childIndexStack, arrayIndexStack, occursCountStack, diagnostics, List(false))
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
  def createInitialState(
    rootElemDecl: GlobalElementDecl,
    input: DFDL.Input,
    bitOffset: Long = 0,
    bitLengthLimit: Long = -1): PState = {
    val inStream =
      InStream.fromByteChannel(rootElemDecl, input, bitOffset, bitLengthLimit)
    createInitialState(rootElemDecl, inStream)
  }

}

