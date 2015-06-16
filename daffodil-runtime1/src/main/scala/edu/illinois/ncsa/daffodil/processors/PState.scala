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
import edu.illinois.ncsa.daffodil.xml.GlobalQName
import java.io.StringReader
import org.apache.commons.io.input.ReaderInputStream
import edu.illinois.ncsa.daffodil.events.EventHandler
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.processors.dfa.DFADelimiter
import java.nio.charset.CharsetDecoder
import java.nio.charset.Charset
import scala.collection.mutable
import java.nio.charset.CharsetEncoder
import edu.illinois.ncsa.daffodil.io.ByteBufferDataInputStream
import edu.illinois.ncsa.daffodil.io.DataInputStream
import edu.illinois.ncsa.daffodil.io.DataStreamCommon

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
  def getAllTerminatingMarkup = delimiterStack.iterator.flatMap { _.getTerminatingMarkup }.toList.toSeq // use list here because toSeq creates a lazy stream from an iterator
  def getAllDelimitersWithPos = delimiterStack.iterator.flatMap { _.getDelimitersWithPos }.toList.toSeq
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
  var dataProc: DataProcessor) extends DFDL.State with ThrowsSDE with SavesErrorsAndWarnings {

  private lazy val myFirstThread = Thread.currentThread()

  protected final def threadCheck() {
    Assert.invariant(Thread.currentThread eq myFirstThread)
  }

  override def schemaFileLocation = getContext().schemaFileLocation

  def dataStream: DataStreamCommon

  def bitPos0b: Long
  def bitLimit0b: Maybe[Long]
  final def bytePos0b = bitPos0b >> 3
  final def bytePos1b = (bitPos0b >> 3) + 1
  final def bitPos1b = bitPos0b + 1
  final def bitLimit1b = bitLimit0b.map { _ + 1 }
  final def whichBit0b = bitPos0b % 8

  // TODO: many off-by-one errors due to not keeping strong separation of 
  // one-based and zero-based indexes.
  // 
  // We could separate these with the type system.
  //
  // So implement a OneBasedBitPos and ZeroBasedBitPos value class with
  // operations that convert between them, allow adding & subtracting only
  // in sensible ways, etc. 
  final def bitPos = bitPos0b
  final def bytePos = bytePos0b
  // def charPos: Long

  def groupPos: Long
  def arrayPos: Long
  def childPos: Long
  def occursBoundsStack: Stack[Long]

  def hasInfoset: Boolean
  def infoset: InfosetItem

  def thisElement: InfosetElement

  def getContext(): ElementRuntimeData = {
    // threadCheck()
    val currentElement = infoset.asInstanceOf[InfosetElement]
    val res = currentElement.runtimeData
    res
  }

  /**
   * Change the bitOrder
   *
   * Must be done at a byte boundary.
   */
  @deprecated("2015-07-22", "Do not call this. Bit order is set by the BitOrderChange parser/unparser")
  def setBitOrder(bitOrder: BitOrder) {
    // threadCheck()
    schemaDefinitionUnless((bitPos1b % 8) == 1,
      "The bitOrder cannot be changed unless the data is aligned at a byte boundary. The bit position (1 based) mod 8 is %s.", bitPos1b)
    // inStream = inStream.withBitOrder(bitOrder)
    dataStream.setBitOrder(bitOrder)
  }

  /**
   * The User API sets the debugger and debug on/off flag on the DataProcessor object.
   * When a PState or UState is created by the DataProcessor, the DataProcessor
   * sets notifies the state object so that it can setup any debug-specific behaviors.
   */
  def notifyDebugging(flag: Boolean): Unit

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

  private val decoderCache = new mutable.HashMap[Charset, CharsetDecoder]()
  private val encoderCache = new mutable.HashMap[Charset, CharsetEncoder]()

  def getDecoder(charset: Charset): CharsetDecoder = {
    // threadCheck()
    var optDecoder = decoderCache.get(charset)
    if (optDecoder.isEmpty) {
      val decoder = charset.newDecoder()
      decoderCache.put(charset, decoder)
      optDecoder = Option(decoder)
    }
    optDecoder.get
  }

  def getEncoder(charset: Charset): CharsetEncoder = {
    // threadCheck()
    var optEncoder = encoderCache.get(charset)
    if (optEncoder.isEmpty) {
      val encoder = charset.newEncoder()
      encoderCache.put(charset, encoder)
      optEncoder = Option(encoder)
    }
    optEncoder.get
  }

}

final class PState private (
  var infoset: InfosetItem,
  var dataInputStream: DataInputStream,
  vmap: VariableMap,
  var status: ProcessorResult,
  diagnosticsArg: List[Diagnostic],
  var discriminatorStack: List[Boolean],
  val mpstate: MPState,
  dataProcArg: DataProcessor,
  var foundDelimiter: Maybe[FoundDelimiterText])
  extends ParseOrUnparseState(mpstate.dstate, vmap, diagnosticsArg, dataProcArg) {

  override def dataStream: DataStreamCommon = dataInputStream

  def withDelimitedText(foundText: String, originalRepresentation: String) {
    // threadCheck()
    val newDelimiter = new FoundDelimiterText(foundText, originalRepresentation) // TODO: Performance - allocates every time
    this.foundDelimiter = One(newDelimiter)
  }

  def clearDelimitedText() {
    // threadCheck()
    this.foundDelimiter = Nope
  }

  override def hasInfoset = true
  def thisElement = infoset.asInstanceOf[InfosetElement]

  override def groupPos = mpstate.groupPos
  override def arrayPos = mpstate.arrayPos
  override def childPos = mpstate.childPos
  override def occursBoundsStack = mpstate.occursBoundsStack

  // No longer a case-class, so we need our own copy routine.
  private def copy(dataInputStream: DataInputStream = dataInputStream,
    infoset: InfosetItem = infoset, variableMap: VariableMap = variableMap, status: ProcessorResult = status,
    diagnostics: List[Diagnostic] = diagnostics,
    discriminatorStack: List[Boolean] = discriminatorStack,
    mpstate: MPState = mpstate, // don't copy the mpstate object itself, just the reference to it.
    dataProc: DataProcessor = dataProc,
    foundDelim: Maybe[FoundDelimiterText] = foundDelimiter) = {
    // threadCheck()
    new PState(infoset, dataInputStream,
      variableMap, status, diagnostics, discriminatorStack, mpstate, dataProc, foundDelim)
  }

  @deprecated("2015-07-15", "must get rid of this copying!")
  private def duplicate() = {
    // threadCheck()
    val res = copy()
    res.dataInputStream = res.dataInputStream.asInstanceOf[ByteBufferDataInputStream].makeACopy
    res
  }

  private def markPool = {
    PState.markPoolTL
  }

  private def getMarkFromPool: PState.Mark =
    {
      // threadCheck()
      if (markPool.isEmpty) One(duplicate())
      else {
        val pstate = markPool.pop().get
        pstate.assignFrom(this) // TODO: Hack - must get rid of this copying.
        pstate.dataInputStream = pstate.dataInputStream.asInstanceOf[ByteBufferDataInputStream].makeACopy
        One(pstate)
      }
    }

  def mark: PState.Mark = {
    // threadCheck()
    getMarkFromPool
  }

  def reset(m: PState.Mark) {
    // threadCheck()
    Assert.usage(m.get.dataInputStream != null)
    this.assignFrom(m.get)
    m.get.clear()
    markPool.push(m)
  }

  def discard(m: PState.Mark) {
    // threadCheck()
    m.get.clear()
  }

  /**
   * Clear so that PState saved on the Mark Stack aren't
   * holding on to a lot of garbage.
   */
  private def clear() = {
    // threadCheck()
    dataInputStream = null
    infoset = null
    variableMap = null
    diagnostics = null
    status = null
    discriminatorStack = null
    dataProc = null
  }

  private def assignFrom(other: PState) {
    // threadCheck()
    if (dataInputStream != null) dataInputStream.asInstanceOf[ByteBufferDataInputStream].assignFrom(other.dataInputStream)
    else dataInputStream = other.dataInputStream
    infoset = other.infoset
    variableMap = other.variableMap
    status = other.status
    diagnostics = other.diagnostics
    discriminatorStack = other.discriminatorStack
    dataProc = other.dataProc
    foundDelimiter = other.foundDelimiter
  }

  override def toString() = {
    // threadCheck()
    "PState( bitPos=%s status=%s )".format(bitPos0b, status)
  }

  def currentLocation: DataLocation =
    new DataLoc(bitPos1b, bitLimit1b, Right(dataInputStream),
      Maybe(thisElement.runtimeData))

  def discriminator = discriminatorStack.head
  def bitPos0b = dataInputStream.bitPos0b
  def bitLimit0b = dataInputStream.bitLimit0b
  //  def charLimit = inStream.charLimit0b

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

  def setEndBitLimit(bitLimit0b: Long) {
    dataInputStream.setBitLimit0b(One(bitLimit0b))
  }

  def setParent(newParent: InfosetItem) {
    this.infoset = newParent
  }

  def setVariable(varQName: GlobalQName, newValue: Any, referringContext: RuntimeData) {
    this.variableMap = variableMap.setVariable(varQName, newValue, referringContext)
  }

  def withValidationError(msg: String, args: Any*) {
    val ctxt = getContext()
    val vde = new ValidationError(Some(ctxt.schemaFileLocation), this, msg, args: _*)
    diagnostics = vde :: diagnostics
  }

  def withValidationErrorNoContext(msg: String, args: Any*) {
    val vde = new ValidationError(None, this, msg, args: _*)
    diagnostics = vde :: diagnostics
  }

  def setFailed(failureDiagnostic: Diagnostic) {
    // threadCheck()
    status = new Failure(failureDiagnostic)
    diagnostics = failureDiagnostic :: diagnostics
  }

  def pushDiscriminator {
    // threadCheck()
    discriminatorStack = false +: discriminatorStack
  }

  def popDiscriminator {
    // threadCheck()
    discriminatorStack = discriminatorStack.tail
  }

  def setDiscriminator(disc: Boolean) {
    // threadCheck()
    discriminatorStack = disc +: discriminatorStack.tail
  }

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
  //  def setPos(bitPos: Long) {
  ////    val newInStream = inStream.withPos(bitPos)
  ////    this.inStream = newInStream
  //    dataInputStream.asInstanceOf[ByteBufferDataInputStream].setBitPos0b(bitPos)
  //  }

  def captureInfosetElementState = {
    // threadCheck()
    thisElement.captureState()
  }

  def restoreInfosetElementState(st: InfosetElementState) = {
    // threadCheck()
    thisElement.restoreState(st)
  }

  //  /**
  //   * calling this forces the entire input into memory
  //   *
  //   */
  //  def lengthInBytes: Long = inStream.lengthInBytes

  final def notifyDebugging(flag: Boolean) {
    // threadCheck()
    dataInputStream.setDebugging(flag)
  }
}

object PState {

  /**
   * A Mark is just a copy of the whole pstate, but encapsulated so that it isn't *exactly* a PState
   * so we can change the implementation someday
   */
  type Mark = One[PState]

  private object PStateMarkPoolTL extends ThreadLocal[mutable.Stack[Mark]] {
    private final def alloc = mutable.Stack[Mark]()
    override protected def initialValue() = alloc
  }
  private def markPoolTL = PStateMarkPoolTL.get()

  /**
   * Initialize the state block given our InStream and a root element declaration.
   */
  def createInitialPState(
    root: ElementRuntimeData,
    dis: DataInputStream,
    dataProc: DFDL.DataProcessor): PState = {

    val doc = Infoset.newDocument(root)
    val variables = dataProc.getVariables
    val status = Success
    val diagnostics = Nil
    val discriminator = false
    val mutablePState = new MPState
    val newState = new PState(doc, dis, variables, status, diagnostics, List(false), mutablePState,
      dataProc.asInstanceOf[DataProcessor], Nope)
    newState
  }

  /**
   * For testing, we can pass in the Infoset pre-constructed.
   */
  def createInitialPState(
    doc: InfosetDocument,
    root: ElementRuntimeData,
    dis: DataInputStream,
    dataProc: DFDL.DataProcessor): PState = {

    val variables = dataProc.getVariables
    val status = Success
    val diagnostics = Nil
    val discriminator = false
    val mutablePState = new MPState

    val newState = new PState(doc, dis, variables, status, diagnostics, List(false), mutablePState,
      dataProc.asInstanceOf[DataProcessor], Nope)
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
    val dis =
      ByteBufferDataInputStream.fromByteChannel(input, bitOffset, bitLengthLimit, bitOrder)
    createInitialPState(root, dis, dataProc)
  }

}

