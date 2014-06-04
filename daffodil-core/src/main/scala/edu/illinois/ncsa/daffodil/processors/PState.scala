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
  var inStream: InStream,
  var infoset: InfosetItem,
  var variableMap: VariableMap,
  var status: ProcessorResult,
  var diagnostics: List[Diagnostic],
  var discriminatorStack: List[Boolean],
  var foundDelimiter: Option[FoundDelimiterText])
  extends DFDL.State with ThrowsSDE {

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
    foundDelimiter = other.foundDelimiter
  }

  def bytePos = bitPos >> 3
  def whichBit = bitPos % 8
  def groupPos = {
    Assert.usage(!GroupIndexStack.get.isEmpty)
    GroupIndexStack.get.top
  }
  def childPos = {
    Assert.usage(!ChildIndexStack.get.isEmpty)
    ChildIndexStack.get.top
  }
  def arrayPos = {
    Assert.usage(!ArrayIndexStack.get.isEmpty)
    ArrayIndexStack.get.top
  }
  def occursBounds = {
    Assert.usage(!OccursBoundsStack.get.isEmpty)
    OccursBoundsStack.get.top
  }

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

    val doc = Infoset.newDocument()
    val variables = dataProc.getVariables
    val status = Success
    val diagnostics = Nil
    val discriminator = false
    val textReader: Option[DFDLCharReader] = None
    val foundDelimiter: Option[FoundDelimiterText] = None
    SchemaComponentRegistry.setup(scr)
    SchemaComponentRegistry.setup(dataProc)
    ArrayIndexStack.setup()
    GroupIndexStack.setup()
    ChildIndexStack.setup()
    OccursBoundsStack.setup()

    val newState = PState(in, doc, variables, status, diagnostics, List(false), foundDelimiter)
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

