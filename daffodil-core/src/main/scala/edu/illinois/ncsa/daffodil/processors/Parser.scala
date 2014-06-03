package edu.illinois.ncsa.daffodil.processors

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

import scala.xml.Node
import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.api.DataLocation
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.api.LocationInSchemaFile
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.dsom.AnnotatedSchemaComponent
import edu.illinois.ncsa.daffodil.dsom.DiagnosticImplMixin
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.dsom.GlobalElementDecl
import edu.illinois.ncsa.daffodil.dsom.RuntimeSchemaDefinitionError
import edu.illinois.ncsa.daffodil.dsom.RuntimeSchemaDefinitionWarning
import edu.illinois.ncsa.daffodil.dsom.SchemaComponent
import edu.illinois.ncsa.daffodil.dsom.SchemaComponentRegistry
import edu.illinois.ncsa.daffodil.dsom.SchemaDefinitionError
import edu.illinois.ncsa.daffodil.dsom.Term
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocatable
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.api._
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.dsom.ValidationError
import edu.illinois.ncsa.daffodil.externalvars.ExternalVariablesLoader
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder

abstract class ProcessingError extends Exception with DiagnosticImplMixin

class ParseError(sc: SchemaComponent, val pstate: Option[PState], kind: String, args: Any*)
  extends ProcessingError {
  override def getLocationsInSchemaFiles: Seq[LocationInSchemaFile] = List(sc)
  override def getDataLocations: Seq[DataLocation] = pstate.map { _.currentLocation }.toList

  def componentText: String = ""

  override def toString = {
    lazy val argsAsString = args.map { _.toString }.mkString(", ")
    //
    // Right here is where we would lookup the symbolic error kind id, and
    // choose a locale-based message string.
    //
    // For now, we'll just do an automatic English message.
    //
    val msg = {
      if (args.size > 0) kind.format(args: _*)
      else kind
    }
    val res = "Parse Error: " + msg +
      componentText +
      "\nSchema context: %s %s".format(sc, sc.locationDescription) +
      pstate.map { ps => "\nData location was preceding %s".format(ps.currentLocation) }.getOrElse("(no data location)")
    res
  }

  override def getMessage = toString
}

class AssertionFailed(sc: SchemaComponent, state: PState, msg: String, details: Option[String] = None)
  extends ParseError(sc, Some(state), "Assertion failed. %s", msg) {
  override def componentText: String = {
    val currentElem = state.infoset

    val parsedValue = currentElem.jdomElt match {
      case Some(jdomElem) => {
        "\nParsed value was: " + {
          if (jdomElem.getChildren().size() > 0) {
            // Complex
            val name = jdomElem.getName()
            "<" + name + ">...</" + name + ">"
          } else {
            // Simple
            currentElem.toBriefXML.toString
          }
        }
      }
      case None => ""
    }
    val finalString = details match {
      case Some(d) => "\nDetails: " + d + parsedValue
      case None => parsedValue
    }
    finalString
  }
}

class ParseAlternativeFailed(sc: SchemaComponent, state: PState, val errors: Seq[Diagnostic])
  extends ParseError(sc, Some(state), "Alternative failed. Reason(s): %s", errors)

class AltParseFailed(sc: SchemaComponent, state: PState,
  diags: Seq[Diagnostic])
  extends ParseError(sc, Some(state), "All alternatives failed. Reason(s): %s", diags) {

  override def getLocationsInSchemaFiles: Seq[LocationInSchemaFile] = diags.flatMap { _.getLocationsInSchemaFiles }

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
abstract class Parser(val context: SchemaComponent) extends Logging with ToBriefXMLImpl {

  def PE(pstate: PState, s: String, args: Any*) = {
    pstate.failed(new ParseError(context, Some(pstate), s, args: _*))
  }

  def processingError(state: PState, str: String, args: Any*) =
    PE(state, str, args) // long form synonym

  protected def parse(pstate: PState): PState

  final def parse1(pstate: PState, context: SchemaComponent): PState = {
    Debugger.before(pstate, this)
    val afterState = parse(pstate)
    if (!(afterState eq pstate)) {
      // they're not the same state object
      // we make them so.
      pstate.assignFrom(afterState)
      pstate.inStream.assignFrom(afterState.inStream)
    }
    Debugger.after(pstate, afterState, this)
    // afterState
    pstate
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
  // TODO: FIXME Bad bad global state. This flag needs to live in the DataProcessor object,
  // or find a way to have it go away entirely.
  var flag: Boolean = false

  /**
   * for unit tests and other context where you want to exercise runtime code that might throw PEs
   * but you are not inside a parser
   */
  def pretendThisIsAParser[T](body: => T) = {
    val savedFlag: Boolean = WithParseErrorThrowing.flag
    try {
      WithParseErrorThrowing.flag = true
      body
    } finally {
      WithParseErrorThrowing.flag = savedFlag
    }
  }
}

// No-op, in case an optimization lets one of these sneak thru. 
// TODO: make this fail, and test optimizer sufficiently to know these 
// do NOT get through.
class EmptyGramParser(context: Term = null) extends Parser(context) {
  def parse(pstate: PState) = Assert.invariantFailed("EmptyGramParsers are all supposed to optimize out!")
  override def toBriefXML(depthLimit: Int = -1) = "<empty/>"
  override def toString = toBriefXML()
}

class ErrorParser(context: Term = null) extends Parser(context) {
  def parse(pstate: PState): PState = Assert.abort("Error Parser")
  override def toBriefXML(depthLimit: Int = -1) = "<error/>"
  override def toString = "Error Parser"
}

/**
 * BriefXML is XML-style output, but intended for specific purposes. It is NOT
 * an XML serialization of the data structure. It's an XML-style string, suitable to
 * manipulate, by people, in XML tooling. E.g., can stick into an XML editor to
 * then get it all indented nicely, use a structure editor to expand/collapse subregions,
 * but it is NOT intended to capture all of the state of the object.
 */
trait ToBriefXMLImpl {

  val nom: String = Misc.getNameFromClass(this)

  val childParsers: Seq[Parser] = Nil

  // TODO: make this do indenting and newlines (maybe optionally?)
  def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..."
    else if (depthLimit == 1 && childParsers.length == 0) "<" + nom + "/>"
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

  override val nom = "seq"

  override val childParsers = children.map { _.parser }

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

  override val nom = "alt"

  override val childParsers = children.map { _.parser }

  def parse(pInitial: PState): PState = {
    val pStart = pInitial.withNewPointOfUncertainty
    var pResult: PState = null
    var diagnostics: Seq[Diagnostic] = Nil
    val cloneNode = pStart.captureInfosetElementState // we must undo side-effects on the JDOM if we backtrack.
    childParsers.foreach { parser =>
      {
        log(LogLevel.Debug, "Trying choice alternative: %s", parser)
        try {
          val ps = pStart.duplicate()
          pResult = parser.parse1(ps, context)
        } catch {
          case u: UnsuppressableException => throw u
          case rsde: RuntimeSchemaDefinitionError => throw rsde
          case e: Exception => Assert.invariantFailed("Runtime parsers should not throw exceptions: " + e)
        }
        if (pResult.status == Success) {
          log(LogLevel.Debug, "Choice alternative success: %s", parser)
          val res = pResult.withRestoredPointOfUncertainty
          return res
        }
        // If we get here, then we had a failure
        log(LogLevel.Debug, "Choice alternative failed: %s", parser)
        // Unwind any side effects on the Infoset 
        // The infoset is the primary non-functional data structure. We have to un-side-effect it.
        pStart.restoreInfosetElementState(cloneNode)
        val diag = new ParseAlternativeFailed(context, pStart, pResult.diagnostics)
        diagnostics = diag +: diagnostics
        // check for discriminator evaluated to true.
        if (pResult.discriminator == true) {
          log(LogLevel.Debug, "Failure, but discriminator true. Additional alternatives discarded.")
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
    log(LogLevel.Debug, "All AltParser alternatives failed.")
    val result = allFailedResult.withRestoredPointOfUncertainty
    result
  }

}

case class DummyParser(sc: SchemaComponent) extends Parser(null) {
  def parse(pstate: PState): PState = sc.SDE("Parser for " + sc + " is not yet implemented.")

  override def toBriefXML(depthLimit: Int = -1) = "<dummy/>"
  override def toString = if (sc == null) "Dummy[null]" else "Dummy[" + sc + "]"
}

class GeneralParseFailure(msg: String) extends Throwable with DiagnosticImplMixin {
  Assert.usage(msg != null && msg != "")
  override def getMessage() = msg
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
case class PState(
  var schemaComponentRegistry: SchemaComponentRegistry,
  var inStream: InStream,
  var infoset: InfosetItem,
  var variableMap: VariableMap,
  var target: NS,
  var status: ProcessorResult,
  var groupIndexStack: List[Long],
  var childIndexStack: List[Long],
  var arrayIndexStack: List[Long],
  var occursCountStack: List[Long],
  var diagnostics: List[Diagnostic],
  var discriminatorStack: List[Boolean],
  var dataProc: DFDL.DataProcessor,
  var foundDelimiter: Option[FoundDelimiterText])
  extends DFDL.State with ThrowsSDE {

  def duplicate() = {
    val res = copy()
    res.inStream = inStream.duplicate()
    res
  }

  def assignFrom(other: PState) {
    schemaComponentRegistry = other.schemaComponentRegistry
    inStream = other.inStream
    infoset = other.infoset
    variableMap = other.variableMap
    target = other.target
    status = other.status
    groupIndexStack = other.groupIndexStack
    childIndexStack = other.childIndexStack
    arrayIndexStack = other.arrayIndexStack
    occursCountStack = other.occursCountStack
    diagnostics = other.diagnostics
    discriminatorStack = other.discriminatorStack
    dataProc = other.dataProc
    foundDelimiter = other.foundDelimiter
  }

  def bytePos = bitPos >> 3
  def whichBit = bitPos % 8
  def groupPos = if (groupIndexStack != Nil) groupIndexStack.head else -1
  def childPos = if (childIndexStack != Nil) childIndexStack.head else -1
  def arrayPos = if (arrayIndexStack != Nil) arrayIndexStack.head else -1
  def occursCount = if (occursCountStack != Nil) occursCountStack.head else -1

  override def toString() = {
    "PState( bitPos=%s charPos=%s status=%s )".format(bitPos, charPos, status)
  }
  def getContext(): ElementBase = {
    // Assumes that a JDOM element was already created
    val currentElement = parentElement
    val res = currentElement.schemaComponent(this)
    res
  }

  /**
   * Added because ThrowsSDE is a SchemaFileLocatable
   */
  def contextLocatable: SchemaFileLocatable = {
    val ctxt = getContext()
    ctxt.contextLocatable
  }

  /**
   * Added because ThrowsSDE is a SchemaFileLocatable
   */
  def fileName: String = {
    val ctxt = getContext()
    ctxt.fileName
  }

  /**
   * Added because ThrowsSDE is a SchemaFileLocatable
   */
  def xml: Node = {
    val ctxt = getContext()
    ctxt.xml
  }

  def SDE(str: String, args: Any*) = {
    ExecutionMode.requireRuntimeMode
    val ctxt = getContext()
    val rsde = new RuntimeSchemaDefinitionError(ctxt, this, str, args: _*)
    ctxt.toss(rsde)
  }

  // TODO: Do we want these to reside on PState at all? SDEButContinue and SDW
  // Had to implement so that we could add ThrowsSDE as a trait to PState
  // Could just make private and Assert.impossible
  def SDEButContinue(id: String, args: Any*): Unit = {
    ExecutionMode.requireRuntimeMode
    val ctxt = getContext
    val rsde = new RuntimeSchemaDefinitionError(ctxt, this, id, args: _*)
    ctxt.error(rsde)
  }

  def SDW(id: String, args: Any*): Unit = {
    ExecutionMode.requireRuntimeMode
    val ctxt = getContext
    val rsdw = new RuntimeSchemaDefinitionWarning(ctxt, this, id, args: _*)
    ctxt.warn(rsdw)
  }

  def discriminator = discriminatorStack.head
  def currentLocation: DataLocation = new DataLoc(bitPos, bitLimit, inStream)
  // def inStreamState = inStreamStateStack top
  def bitPos = inStream.bitPos
  def bitLimit = inStream.bitLimit
  def charPos = inStream.charPos
  def charLimit = inStream.charLimit
  def parentElement = infoset.asInstanceOf[InfosetElement]
  def parentDocument = infoset.asInstanceOf[InfosetDocument]
  def textReader = inStream.reader

  /**
   * Convenience functions for creating a new state, changing only
   * one or a related subset of the state components to a new one.
   */

  //  def withPos(bitPos: Long, charPos: Long, newStatus: ProcessorResult = Success) = {
  //    val newInStream = inStream.withPos(bitPos, charPos)
  //    copy(inStream = newInStream, status = newStatus)
  //  }

  def withEndBitLimit(bitLimit: Long, newStatus: ProcessorResult = this.status) = {
    if (bitLimit == inStream.bitLimit) { this } // already have this bitLimit.
    else {
      var newInStream = inStream.withEndBitLimit(bitLimit)
      // copy(inStream = newInStream, status = newStatus)
      this.inStream = newInStream
      this.status = newStatus
    }
    this
  }

  def withParent(newParent: InfosetItem, newStatus: ProcessorResult = Success) = {
    //    copy(infoset = newParent, status = newStatus)
    this.infoset = newParent
    this.status = newStatus
    this
  }

  def withVariables(newVariableMap: VariableMap, newStatus: ProcessorResult = Success) = {
    //    copy(variableMap = newVariableMap, status = newStatus)
    this.variableMap = newVariableMap
    this.status = newStatus
    this
  }

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

  def withValidationError(msg: String, args: Any*) = {
    val ctxt = getContext()
    val vde = new ValidationError(Some(ctxt), this, msg, args: _*)
    // copy(diagnostics = vde :: diagnostics)
    diagnostics = vde :: diagnostics
    this
  }
  def withValidationErrorNoContext(msg: String, args: Any*) = {
    val vde = new ValidationError(None, this, msg, args: _*)
    copy(diagnostics = vde :: diagnostics)
  }

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

  /**
   * withPos changes the bit position of the stream, and maintains the char reader
   * which is available to decode characters at that position.
   *
   * It is critical to performance that the reader be preserved if it can be. That is, if we are
   * moving through characters of text in the same encoding, with no binary data or alignment going on, then
   * we *must* retain the reader. Creating a new reader has high overhead in that as soon as you create one and
   * read anything from it, it will read-ahead a large block of characters. If every element was creating
   * a new reader, we'd be reading data over and over again.
   *
   * So it is NOT ok to just pass None as the third argument. Only do that if you have
   * just been handling binary data, or just did an alignmentFill that really inserted some bits.
   *
   * It is well worth it to test and branch to preserve the reader. E.g., AlignmentFill should not
   * create a new reader unless it actually moved over some number of bits. If the alignment is 1 (bit),
   * or the actual amount of alignment fill to be skipped in a particular data stream is 0, then
   * one should preserve the reader.
   *
   * This method mostly just delegates to the inStream now. But the caller of this method
   * needs to avoid just passing None also. So this Scaladoc appears both here and on the withPos
   * method of inStream.
   */
  def withPos(bitPos: Long, charPos: Long, reader: Option[DFDLCharReader]) = {
    val newInStream = inStream.withPos(bitPos, charPos, reader)
    //    copy(inStream = newInStream)
    this.inStream = newInStream
    this
  }

  def withDelimitedText(foundText: String, originalRepresentation: String) = {
    val newDelimiter = new FoundDelimiterText(foundText, originalRepresentation)
    copy(foundDelimiter = Some(newDelimiter))
  }
  def clearDelimitedText() = {
    copy(foundDelimiter = None)
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

  /**
   * calling this forces the entire input into memory
   *
   */
  def lengthInBytes: Long = inStream.lengthInBytes

  /**
   * Change the bitOrder
   *
   * Must be done at a byte boundary.
   */

  def withBitOrder(bitOrder: BitOrder) = {
    schemaDefinitionUnless((bitPos % 8) == 1, "The bitOrder cannot be changed unless the data is aligned at a byte boundary. The bit position mod 8 is %s.", bitPos)
    copy(inStream = inStream.withBitOrder(bitOrder))
  }
}

object PState {

  /**
   * Initialize the state block given our InStream and a root element declaration.
   */
  def createInitialState(scr: SchemaComponentRegistry,
    rootElemDecl: GlobalElementDecl,
    in: InStream,
    dataProc: DFDL.DataProcessor): PState = {

    val dataProcessor = dataProc
    val doc = Infoset.newDocument()
    val variables = dataProc.getVariables
    val targetNamespace = rootElemDecl.schemaDocument.targetNamespace
    val status = Success
    val groupIndexStack = Nil
    val childIndexStack = Nil
    val arrayIndexStack = Nil
    val occursCountStack = Nil
    val diagnostics = Nil
    val discriminator = false
    val textReader: Option[DFDLCharReader] = None
    val foundDelimiter: Option[FoundDelimiterText] = None
    val newState = PState(scr, in, doc, variables, targetNamespace, status, groupIndexStack,
      childIndexStack, arrayIndexStack, occursCountStack, diagnostics, List(false), dataProc, foundDelimiter)
    newState
  }

  /**
   * For testing it is convenient to just hand it strings for data.
   */
  def createInitialState(scr: SchemaComponentRegistry,
    rootElemDecl: GlobalElementDecl,
    data: String,
    bitOffset: Long,
    dataProc: DFDL.DataProcessor): PState = {
    val in = Misc.stringToReadableByteChannel(data)
    createInitialState(scr, rootElemDecl, in, dataProc, data.length, bitOffset)
  }

  /**
   * Construct our InStream object and initialize the state block.
   */
  def createInitialState(scr: SchemaComponentRegistry,
    rootElemDecl: GlobalElementDecl,
    input: DFDL.Input,
    dataProc: DFDL.DataProcessor,
    bitOffset: Long = 0,
    bitLengthLimit: Long = -1): PState = {
    val bitOrder = rootElemDecl.bitOrder
    val inStream =
      InStream.fromByteChannel(rootElemDecl, input, bitOffset, bitLengthLimit, bitOrder)
    createInitialState(scr, rootElemDecl, inStream, dataProc)
  }

}

