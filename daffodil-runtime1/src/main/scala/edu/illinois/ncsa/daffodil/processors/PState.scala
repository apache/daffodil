/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors

import scala.xml.Node
import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.api.DataLocation
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.api.LocationInSchemaFile
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.dsom.DiagnosticImplMixin
import edu.illinois.ncsa.daffodil.dsom.RuntimeSchemaDefinitionError
import edu.illinois.ncsa.daffodil.dsom.RuntimeSchemaDefinitionWarning
import edu.illinois.ncsa.daffodil.dsom.SchemaDefinitionError
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocatable
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.exceptions.SavesErrorsAndWarnings
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.api._
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.dsom.ValidationError
import edu.illinois.ncsa.daffodil.externalvars.ExternalVariablesLoader
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import scala.collection.mutable.Stack
import edu.illinois.ncsa.daffodil.dpath.DState
import edu.illinois.ncsa.daffodil.processors.dfa.DFAField
import scala.collection.mutable.ArrayStack
import edu.illinois.ncsa.daffodil.processors.unparsers.UnparseError

case class MPState() {

  val dstate: DState = new DState
  val arrayIndexStack = Stack[Long](1L)
  def moveOverOneArrayIndexOnly() = arrayIndexStack.push(arrayIndexStack.pop + 1)
  def arrayPos = arrayIndexStack.top

  val groupIndexStack = Stack[Long](1L)
  def moveOverOneGroupIndexOnly() = groupIndexStack.push(groupIndexStack.pop + 1)
  def groupPos = groupIndexStack.top

  // TODO: it doesn't look anything is actually reading the value of childindex
  // stack. Can we get rid of it?
  val childIndexStack = Stack[Long](1L)
  def moveOverOneElementChildOnly() = childIndexStack.push(childIndexStack.pop + 1)
  def childPos = childIndexStack.top

  val occursBoundsStack = new Stack[Long]
  def updateBoundsHead(ob: Long) = {
    occursBoundsStack.pop()
    occursBoundsStack.push(ob)
  }
  def occursBounds = occursBoundsStack.top
  
  var currentEscapeScheme: Maybe[EscapeSchemeParserHelper] = Nope

  val delimiterStack = new ArrayStack[DelimiterStackNode]()
  def pushDelimiters(node: DelimiterStackNode) = delimiterStack.push(node)
  def popDelimiters() = delimiterStack.pop
  def localDelimiters = delimiterStack.top
  def remoteDelimiters = delimiterStack.tail
  def getAllTerminatingMarkup = delimiterStack.iterator.flatMap { _.getTerminatingMarkup }.toSeq
  def getAllDelimitersWithPos = delimiterStack.iterator.flatMap { _.getDelimitersWithPos }.toSeq

  var foundDelimiter: Maybe[FoundDelimiterText] = Nope

  def withDelimitedText(foundText: String, originalRepresentation: String) {
    val newDelimiter = new FoundDelimiterText(foundText, originalRepresentation)
    this.foundDelimiter = One(newDelimiter)
  }
  def clearDelimitedText() {
    this.foundDelimiter = Nope
  }
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
abstract class ParseOrUnparseState(
  var dstate: DState,
  var variableMap: VariableMap,
  var diagnostics: List[Diagnostic],
  var dataProc: DataProcessor)
  extends DFDL.State with ThrowsSDE with SavesErrorsAndWarnings {
  override def schemaFileLocation = getContext().schemaFileLocation

  def bitPos1b: Long
  def bitLimit1b: Long

  def thisElement: InfosetElement

  def getContext(): ElementRuntimeData = {
    val currentElement = thisElement
    val res = currentElement.runtimeData
    res
  }
  def SDE(str: String, args: Any*) = {
    ExecutionMode.requireRuntimeMode
    val ctxt = getContext()
    val rsde = new RuntimeSchemaDefinitionError(ctxt.schemaFileLocation, this, str, args: _*)
    ctxt.toss(rsde)
  }

  def SDEButContinue(str: String, args: Any*) = {
    ExecutionMode.requireRuntimeMode
    val ctxt = getContext()
    val rsde = new RuntimeSchemaDefinitionError(ctxt.schemaFileLocation, this, str, args: _*)
    diagnostics = rsde :: diagnostics
  }

  def SDW(str: String, args: Any*) = {
    ExecutionMode.requireRuntimeMode
    val ctxt = getContext()
    val rsdw = new RuntimeSchemaDefinitionWarning(ctxt.schemaFileLocation, this, str, args: _*)
    diagnostics = rsdw :: diagnostics
  }

  def addUnparseError(ue: UnparseError) {
    diagnostics = ue :: diagnostics
  }
}

class PState(
  var infoset: InfosetItem,
  var inStream: InStream,
  vmap: VariableMap,
  var status: ProcessorResult,
  diagnosticsArg: List[Diagnostic],
  var discriminatorStack: List[Boolean],
  val mpstate: MPState,
  dataProcArg: DataProcessor)
  extends ParseOrUnparseState(mpstate.dstate, vmap, diagnosticsArg, dataProcArg) {

  def thisElement = infoset.asInstanceOf[InfosetElement]

  // No longer a case-class, so we need our own copy routine... since PStates get copied 
  // a lot.
  def copy(inStream: InStream = inStream,
    infoset: InfosetItem = infoset, variableMap: VariableMap = variableMap, status: ProcessorResult = status,
    diagnostics: List[Diagnostic] = diagnostics,
    discriminatorStack: List[Boolean] = discriminatorStack,
    mpstate: MPState = mpstate, // don't copy the mpstate object itself, just the reference to it.
    dataProc: DataProcessor = dataProc) =
    new PState(infoset, inStream,
      variableMap, status, diagnostics, discriminatorStack, mpstate, dataProc)
  // TODO: many off-by-one errors due to not keeping strong separation of 
  // one-based and zero-based indexes.
  // 
  // We could separate these with the type system.
  //
  // So implement a OneBasedBitPos and ZeroBasedBitPos value class with
  // operations that convert between them, allow adding & subtracting only
  // in sensible ways, etc. 
  def bitPos = bitPos0b
  def bytePos = bytePos0b

  def duplicate() = {
    val res = copy()
    res.inStream = inStream.duplicate()
    res
  }

  def assignFrom(other: PState) {
    inStream = other.inStream
    infoset = other.infoset
    variableMap = other.variableMap
    status = other.status
    diagnostics = other.diagnostics
    discriminatorStack = other.discriminatorStack
    dataProc = other.dataProc
  }

  def bytePos0b = bitPos0b >> 3
  def bytePos1b = (bitPos0b >> 3) + 1
  def bitPos1b = bitPos0b + 1
  def bitLimit1b = bitLimit0b + 1
  def whichBit = bitPos0b % 8

  override def toString() = {
    "PState( bitPos=%s charPos=%s status=%s )".format(bitPos0b, charPos, status)
  }

  def currentLocation: DataLocation = new DataLoc(bitPos1b, bitLimit1b, inStream)

  def discriminator = discriminatorStack.head
  // def inStreamState = inStreamStateStack top
  def bitPos0b = inStream.bitPos0b
  def bitLimit0b = inStream.bitLimit0b
  def charPos = inStream.charPos0b
  def charLimit = inStream.charLimit0b

  def simpleElement: InfosetSimpleElement = {
    val res = infoset match {
      case s: DISimple => s
      case _ => Assert.usageError("not a simple element")
    }
    res
  }

  def complexElement: InfosetComplexElement = {
    val res = infoset match {
      case c: DIComplex => c
      case _ => Assert.usageError("not a complex element.")
    }
    res
  }
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

  def withEndBitLimit(bitLimit0b: Long, newStatus: ProcessorResult = this.status) = {
    if (bitLimit0b == inStream.bitLimit0b) { this } // already have this bitLimit.
    else {
      var newInStream = inStream.withEndBitLimit(bitLimit0b)
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

  def withValidationError(msg: String, args: Any*) = {
    val ctxt = getContext()
    val vde = new ValidationError(Some(ctxt.schemaFileLocation), this, msg, args: _*)
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
    copy(status = new Failure(failureDiagnostic),
      diagnostics = failureDiagnostic :: diagnostics)
  }

  def setFailed(failureDiagnostic: Diagnostic) {
    status = new Failure(failureDiagnostic)
    diagnostics = failureDiagnostic :: diagnostics
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
  def withPos(bitPos: Long, charPos: Long, reader: Maybe[DFDLCharReader]) = {
    val newInStream = inStream.withPos(bitPos, charPos, reader)
    //    copy(inStream = newInStream)
    this.inStream = newInStream
    this
  }

  def captureInfosetElementState = thisElement.captureState()

  def restoreInfosetElementState(st: InfosetElementState) = thisElement.restoreState(st)

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
    schemaDefinitionUnless((bitPos1b % 8) == 1,
      "The bitOrder cannot be changed unless the data is aligned at a byte boundary. The bit position (1 based) mod 8 is %s.", bitPos1b)
    copy(inStream = inStream.withBitOrder(bitOrder))
  }
}

object PState {

  /**
   * Initialize the state block given our InStream and a root element declaration.
   */
  def createInitialPState(
    root: ElementRuntimeData,
    in: InStream,
    dataProc: DFDL.DataProcessor): PState = {

    val doc = Infoset.newDocument(root)
    val variables = dataProc.getVariables
    val status = Success
    val diagnostics = Nil
    val discriminator = false
    val textReader: Maybe[DFDLCharReader] = Nope
    val mutablePState = new MPState
    val newState = new PState(doc, in, variables, status, diagnostics, List(false), mutablePState,
      dataProc.asInstanceOf[DataProcessor])
    newState
  }

  /**
   * For testing, we can pass in the Infoset pre-constructed.
   */
  def createInitialPState(
    doc: InfosetDocument,
    root: ElementRuntimeData,
    in: InStream,
    dataProc: DFDL.DataProcessor): PState = {

    val variables = dataProc.getVariables
    val status = Success
    val diagnostics = Nil
    val discriminator = false
    val textReader: Maybe[DFDLCharReader] = Nope
    val mutablePState = new MPState

    val newState = new PState(doc, in, variables, status, diagnostics, List(false), mutablePState,
      dataProc.asInstanceOf[DataProcessor])
    newState
  }

  /**
   * For testing it is convenient to just hand it strings for data.
   */
  def createInitialPState(
    root: ElementRuntimeData,
    data: String,
    bitOffset: Long,
    dataProc: DFDL.DataProcessor): PState = {
    val in = Misc.stringToReadableByteChannel(data)
    createInitialPState(root, in, dataProc, data.length, bitOffset)
  }

  /**
   * Construct our InStream object and initialize the state block.
   */
  def createInitialPState(
    root: ElementRuntimeData,
    input: DFDL.Input,
    dataProc: DFDL.DataProcessor,
    bitOffset: Long = 0,
    bitLengthLimit: Long = -1): PState = {
    val bitOrder = root.defaultBitOrder
    val inStream =
      InStream.fromByteChannel(root, input, bitOffset, bitLengthLimit, bitOrder)
    createInitialPState(root, inStream, dataProc)
  }

}

