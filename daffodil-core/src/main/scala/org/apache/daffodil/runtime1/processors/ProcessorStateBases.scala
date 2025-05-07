/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.runtime1.processors

import java.nio.CharBuffer
import java.nio.LongBuffer

import org.apache.daffodil.api
import org.apache.daffodil.io.DataStreamCommon
import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.io.LocalBufferMixin
import org.apache.daffodil.io.processors.charset.BitsCharsetDecoder
import org.apache.daffodil.io.processors.charset.BitsCharsetEncoder
import org.apache.daffodil.io.processors.charset.CoderInfo
import org.apache.daffodil.io.processors.charset.DecoderInfo
import org.apache.daffodil.io.processors.charset.EncoderDecoderMixin
import org.apache.daffodil.io.processors.charset.EncoderInfo
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.SavesErrorsAndWarnings
import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.lib.iapi.DaffodilTunables
import org.apache.daffodil.lib.iapi.DataLocation
import org.apache.daffodil.lib.iapi.Diagnostic
import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.lib.schema.annotation.props.gen.BinaryFloatRep
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.schema.annotation.props.gen.ByteOrder
import org.apache.daffodil.lib.schema.annotation.props.gen.EncodingErrorPolicy
import org.apache.daffodil.lib.schema.annotation.props.gen.UTF16Width
import org.apache.daffodil.lib.util.MStackOfLong
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.lib.util.MaybeInt
import org.apache.daffodil.lib.util.MaybeULong
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.runtime1.dpath.DState
import org.apache.daffodil.runtime1.dsom.DPathCompileInfo
import org.apache.daffodil.runtime1.dsom.RuntimeSchemaDefinitionError
import org.apache.daffodil.runtime1.dsom.RuntimeSchemaDefinitionWarning
import org.apache.daffodil.runtime1.dsom.SchemaDefinitionErrorFromWarning
import org.apache.daffodil.runtime1.dsom.ValidationError
import org.apache.daffodil.runtime1.iapi.DFDL
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.infoset._
import org.apache.daffodil.runtime1.processors.dfa.Registers
import org.apache.daffodil.runtime1.processors.dfa.RegistersPool
import org.apache.daffodil.runtime1.processors.parsers.PState
import org.apache.daffodil.runtime1.processors.parsers.ParseError
import org.apache.daffodil.runtime1.processors.unparsers.UState
import org.apache.daffodil.runtime1.processors.unparsers.UnparseError

/**
 * Trait mixed into the PState.Mark object class and the ParseOrUnparseState
 *
 * contains member functions for everything the debugger needs to be able to observe.
 */
trait StateForDebugger {
  def currentLocation: DataLocation
  def bitPos0b: Long
  def bitLimit0b: MaybeULong
  def childPos: Long
  def groupPos: Long
  def arrayIterationPos: Long
  def occursPos: Long
  def variableMapForDebugger: VariableMap
  def delimitedParseResult: Maybe[dfa.ParseResult]
  def withinHiddenNest: Boolean
  def suspensions: Seq[Suspension]
}

case class TupleForDebugger(
  val currentLocation: DataLocation,
  val bitPos0b: Long,
  val bitLimit0b: MaybeULong,
  val childPos: Long,
  val groupPos: Long,
  val arrayIterationPos: Long,
  val occursPos: Long,
  val variableMapForDebugger: VariableMap,
  val delimitedParseResult: Maybe[dfa.ParseResult],
  val withinHiddenNest: Boolean,
  val suspensions: Seq[Suspension]
) extends StateForDebugger

trait SetProcessorMixin {
  private var maybeProcessor_ : Maybe[Processor] = Nope

  final def maybeProcessor = maybeProcessor_

  final def processor = {
    Assert.usage(maybeProcessor_.isDefined) // failure means setProcessor wasn't called.
    maybeProcessor_.value
  }

  /**
   * Must be called on the state before any
   * call to the I/O layer, as that will call back to get property-related
   * information and that has to be obtained via the runtime data for the
   * term - whether element or model group. The
   */
  final def setProcessor(p: Processor): Unit = {
    maybeProcessor_ = One(p)
  }

  final def setMaybeProcessor(mp: Maybe[Processor]): Unit = {
    maybeProcessor_ = mp
  }
}

trait HasTunable {
  def tunable: DaffodilTunables
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
abstract class ParseOrUnparseState protected (
  protected var variableBox: VariableBox,
  var diagnostics: java.util.List[api.Diagnostic],
  var dataProc: Maybe[DataProcessor],
  val tunable: DaffodilTunables
) extends DFDL.State
  with StateForDebugger
  with ThrowsSDE
  with SavesErrorsAndWarnings
  with LocalBufferMixin
  with EncoderDecoderMixin
  with FormatInfo
  with SetProcessorMixin {

  def this(
    vmap: VariableMap,
    diags: java.util.List[api.Diagnostic],
    dataProc: Maybe[DataProcessor],
    tunable: DaffodilTunables
  ) =
    this(new VariableBox(vmap), diags, dataProc, tunable)

  def infoset: DIElement

  private def simpleElement = infoset.asInstanceOf[DISimple]

  /*
   * Implement the FormatInfo trait needed by the I/O layer.
   */

  /*
   * Slots that cache the value of these FormatInfo members so that
   * they are recomputed only once per call to parse() or unparse() method.
   */
  private var binaryFloatRepCache: BinaryFloatRep = null
  private var bitOrderCache: BitOrder = null
  private var byteOrderCache: ByteOrder = null
  private var maybeCachedFillByte: MaybeInt = MaybeInt.Nope
  private var decoderCache: BitsCharsetDecoder = null
  private var encoderCache: BitsCharsetEncoder = null
  private var decoderCacheEntry_ : DecoderInfo = null
  private var encoderCacheEntry_ : EncoderInfo = null

  /**
   * Must call after all processors so as to recompute the formatInfo slots
   * for the next parser - since the values could change.
   */
  final def resetFormatInfoCaches(): Unit = {
    binaryFloatRepCache = null
    bitOrderCache = null
    byteOrderCache = null
    maybeCachedFillByte = MaybeInt.Nope
    decoderCache = null
    encoderCache = null
    decoderCacheEntry_ = null
    encoderCacheEntry_ = null
  }

  final def binaryFloatRep: BinaryFloatRep = {
    if (binaryFloatRepCache eq null) {
      binaryFloatRepCache = simpleElement.erd.maybeBinaryFloatRepEv.get.evaluate(this)
    }
    binaryFloatRepCache
  }

  private def runtimeData = processor.context
  private def termRuntimeData = runtimeData.asInstanceOf[TermRuntimeData]

  /**
   * Checks if the bit order change is legal.
   *
   * For parsing we know the bitPos, so we can determine if we're at a byte boundary.
   *
   * For unparsing we may not know the absolute bitPos, so we cannot necessarily
   * determine if the boundary is legal or not.
   *
   * If we know the absoluteBitPos we do the check (as for parsing).
   *
   * If we do not know the absoluteBitPos, then in that case, we split the
   * DataOutputStream into original and buffered. The "check" then occurs
   * when these DataOutputStreams are collapsed back together.
   *
   * So this "check" call, can have an important side effect when unparsing that
   * queues up the check to be done in the future.
   */
  protected def checkBitOrder(): Unit

  /**
   * Returns bit order. If text, this is the bit order for the character set
   * encoding. If binary, this is the bitOrder property value.
   */
  final def bitOrder: BitOrder = {
    if (bitOrderCache eq null) {
      val res = processor match {
        case txtProc: TextProcessor =>
          encoder.bitsCharset.requiredBitOrder
        case _ =>
          processor.context match {
            case trd: TermRuntimeData => trd.defaultBitOrder
            case ntrd: NonTermRuntimeData =>
              Assert.usageError(
                "Cannot ask for bitOrder for non-terms - NonTermRuntimeData: " + ntrd
              )
          }
      }
      bitOrderCache = res
      checkBitOrder()
    }
    bitOrderCache
  }

  final def byteOrder: ByteOrder = {
    if (byteOrderCache eq null) {
      val bo = runtimeData match {
        case erd: ElementRuntimeData => {
          if (erd.maybeByteOrderEv.isDefined) {
            erd.maybeByteOrderEv.get.evaluate(this)
          } else {
            // If byte order is not defined, that means this primitive type is
            // something like hexBinary where byte order should be ignored. We
            // achieve the expected behavior by just setting the byte order to
            // BigEndian.
            ByteOrder.BigEndian
          }
        }
        case mgrd: ModelGroupRuntimeData => {
          //
          // Model Groups can't have the byteOrder property.
          //
          // However, I/O layer still requests it because alignment regions
          // use skip, which ultimately uses getLong/putLong, which asks for
          // byteOrder. (Not any more for 1-byte case - mikeb)
          //
          // A model group DOES care about bit order for its alignment regions,
          // and for the charset encoding of say, initiators or prefix separators.
          // A bitOrder change requires that we check the new bitOrder against the
          // byte order to insure compatibility. (byteOrder can be an expression),
          // so of necessity, we also need byte order.
          //
          // Because the binary integer unparsers (unsignedLong in particular)
          // dispatch on byte order first, then bit order (if littleEndian),
          // we do have to provide the byte order that is consistent with the
          // bit order - if the bit order is LSBF, we have to provide byte order
          // of LittleEndian, otherwise we end up in BigEndian+MSBF code paths.
          //
          val bo =
            if (mgrd.defaultBitOrder eq BitOrder.LeastSignificantBitFirst)
              ByteOrder.LittleEndian
            else
              ByteOrder.BigEndian
          bo
        }
        case _ => Assert.usageError("byte order of non term: " + runtimeData)
      }
      byteOrderCache = bo
    }
    byteOrderCache
  }

  final def maybeCharWidthInBits: MaybeInt = { coderEntry.maybeCharWidthInBits }
  final def encodingMandatoryAlignmentInBits: Int = {
    coderEntry.encodingMandatoryAlignmentInBits
  }
  final def maybeUTF16Width: Maybe[UTF16Width] = termRuntimeData.encodingInfo.maybeUTF16Width

  final def fillByte: Byte = {
    if (maybeCachedFillByte.isEmpty)
      maybeCachedFillByte = MaybeInt(termRuntimeData.fillByteEv.evaluate(this))

    maybeCachedFillByte.get.toByte
  }

  private def getDecoder() = {
    decoderEntry.coder
  }

  final def decoder = {
    if (decoderCache eq null)
      decoderCache = getDecoder()
    decoderCache
  }

  private def getEncoder() = {
    val ee = encoderEntry
    if (encodingErrorPolicy eq EncodingErrorPolicy.Error)
      ee.reportingCoder
    else
      ee.replacingCoder
  }

  final def encoder = {
    if (encoderCache eq null)
      encoderCache = getEncoder()
    encoderCache
  }

  final def encodingErrorPolicy: EncodingErrorPolicy = {
    val eep = termRuntimeData.encodingInfo.defaultEncodingErrorPolicy
    eep
  }

  private def coderEntry: CoderInfo = {
    if (this.isInstanceOf[UState]) encoderEntry
    else decoderEntry
  }

  private def decoderEntry = {
    if (decoderCacheEntry_ eq null) {
      val nextEntry = termRuntimeData.encodingInfo.getDecoderInfo(this)
      decoderCacheEntry_ = nextEntry
      if (this.processor.isPrimitive)
        if (decoderCacheEntry_.encodingMandatoryAlignmentInBitsArg != 1)
          if (this.bitPos1b % 8 != 1)
            checkBitOrder()
    }
    decoderCacheEntry_
  }

  private def encoderEntry = {
    if (encoderCacheEntry_ eq null) {
      val nextEntry = termRuntimeData.encodingInfo.getEncoderInfo(this)
      encoderCacheEntry_ = nextEntry
      if (this.processor.isPrimitive)
        if (encoderCacheEntry_.encodingMandatoryAlignmentInBitsArg != 1)
          if (this.bitPos1b % 8 != 1)
            checkBitOrder()
    }
    encoderCacheEntry_
  }

  /**
   * The variable map is accessed via methods below to set/get and create/remove instances.
   *
   * Everything should access the variables via these methods, not by using the variableMap object
   * directly.
   *
   * The exception to this is the interactive debugger.
   */
  final def variableMap = variableBox.vmap
  final def setVariableMap(newMap: VariableMap): Unit = {
    variableBox.setVMap(newMap)
  }

  final def initializeVariables(): Unit = {
    variableMap.forceExpressionEvaluations(this)
    variableMap.setFirstInstanceInitialValues()
  }

  def setVariable(
    vrd: VariableRuntimeData,
    newValue: DataValuePrimitive,
    referringContext: ThrowsSDE
  ): Unit

  def getVariable(vrd: VariableRuntimeData, referringContext: ThrowsSDE): DataValuePrimitive

  def newVariableInstance(vrd: VariableRuntimeData): VariableInstance

  def removeVariableInstance(vrd: VariableRuntimeData): Unit

  /**
   * The interactive debugger can access the variableMap object state directly.
   *
   * Everthing else should get/set variables using methods on the PState/UState objects.
   */
  final def variableMapForDebugger = variableMap

  final var _processorStatus: ProcessorResult = Success
  final var _validationStatus: Boolean = true

  final def processorStatus = _processorStatus
  final def validationStatus = _validationStatus

  final def isSuccess = processorStatus.isSuccess
  final def isFailure = processorStatus.isFailure

  final def setFailed(failureDiagnostic: api.Diagnostic): Unit = {
    // threadCheck()
    if (!diagnostics.contains(failureDiagnostic)) {
      _processorStatus = new Failure(failureDiagnostic)
      diagnostics = Misc.prependItemToJavaList(failureDiagnostic, diagnostics)
    } else {
      Assert.invariant(processorStatus ne Success)
    }
  }

  final def validationError(msg: String, args: Any*): Unit = {
    val ctxt = getContext()
    val vde = new ValidationError(ctxt.schemaFileLocation, this, msg, args: _*)
    _validationStatus = false
    diagnostics = Misc.prependItemToJavaList(vde, diagnostics)
  }

  final def validationErrorNoContext(cause: Throwable): Unit = {
    val vde = new ValidationError(this, cause)
    _validationStatus = false
    diagnostics = Misc.prependItemToJavaList(vde, diagnostics)
  }

  /**
   * Important: If an error is being suppressed, you must call this to reset the state
   * back so that the prior failure doesn't "last forever" past the point where it is being suppressed.
   *
   * This happens, for example, in the debugger when it is evaluating expressions.
   */
  final def setSuccess(): Unit = {
    _processorStatus = Success
  }

  /**
   * Used when errors are caught by interactive debugger expression evaluation.
   * We don't want to accumulate the diagnostics that we're suppressing.
   */
  final def suppressDiagnosticAndSucceed(d: Diagnostic): Unit = {
    Assert.usage(diagnostics.contains(d))
    diagnostics = diagnostics.filterNot { _ eq d }
    setSuccess()
  }

  def currentNode: Maybe[DINode]

  private val maybeSsrd = if (dataProc.isDefined) { One(dataProc.get.ssrd) }
  else Maybe.Nope

  private val _dState = new DState(maybeSsrd, tunable, One(this))

  /**
   * Used when evaluating expressions. Holds state of expression
   * during evaluation.
   *
   * Doesn't hold every bit of state - that is to say there's still the
   * regular execution call stack, which
   * keeps track of exactly where in the expression evaluation we are.
   */
  final def dState = _dState

  def copyStateForDebugger = {
    TupleForDebugger(
      currentLocation,
      bitPos0b,
      bitLimit0b,
      childPos,
      groupPos,
      arrayIterationPos,
      occursPos,
      variableMap.copy(), // deep copy since variableMap is mutable
      delimitedParseResult,
      withinHiddenNest,
      suspensions
    )
  }

  final override def schemaFileLocation = getContext().schemaFileLocation

  def dataStream: Maybe[DataStreamCommon]

  def bitPos0b: Long
  def bitLimit0b: MaybeULong
  final def bytePos0b = bitPos0b >> 3
  final def bytePos1b = (bitPos0b >> 3) + 1
  final def bitPos1b = bitPos0b + 1
  final def bitLimit1b =
    if (bitLimit0b.isDefined) MaybeULong(bitLimit0b.get + 1) else MaybeULong.Nope
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

  def groupPos: Long
  def arrayIterationPos: Long
  def occursPos: Long
  def childPos: Long

  def hasInfoset: Boolean

  final def maybeERD = {
    if (hasInfoset)
      Maybe(getContext())
    else
      Nope
  }

  def thisElement: api.InfosetElement

  final def getContext(): ElementRuntimeData = {
    // threadCheck()
    val currentElement = infoset
    val res = currentElement.runtimeData
    res
  }

  private var _hiddenDepth = 0

  def incrementHiddenDef(): Unit = {
    _hiddenDepth += 1
  }
  def decrementHiddenDef(): Unit = {
    _hiddenDepth -= 1
  }

  def withinHiddenNest: Boolean = _hiddenDepth > 0

  /**
   * The User API sets the debugger and debug on/off flag on the DataProcessor object.
   * When a PState or UState is created by the DataProcessor, the DataProcessor
   * sets notifies the state object so that it can setup any debug-specific behaviors.
   */
  def notifyDebugging(flag: Boolean): Unit

  final def SDE(str: String, args: Any*) = {
    val ctxt = getContext()
    val rsde = new RuntimeSchemaDefinitionError(ctxt.schemaFileLocation, str, args: _*)
    ctxt.toss(rsde)
  }

  final def SDEButContinue(str: String, args: Any*) = {
    val ctxt = getContext()
    val rsde = new RuntimeSchemaDefinitionError(ctxt.schemaFileLocation, str, args: _*)
    diagnostics = Misc.prependItemToJavaList(rsde, diagnostics)
  }

  final def SDW(warnID: WarnID, str: String, args: Any*) = {
    val ctxt = getContext()
    val lssdw = ctxt.localSuppressSchemaDefinitionWarnings
    val tssdw = tunable.suppressSchemaDefinitionWarnings
    val suppress = lssdw.contains(warnID) || lssdw.contains(WarnID.All) ||
      tssdw.contains(warnID) || tssdw.contains(WarnID.All)
    if (!suppress) {
      val rsdw =
        new RuntimeSchemaDefinitionWarning(warnID, ctxt.schemaFileLocation, str, args: _*)
      if (tunable.escalateWarningsToErrors) {
        val sde = new SchemaDefinitionErrorFromWarning(rsdw)
        ctxt.toss(sde)
      } else {
        diagnostics = Misc.prependItemToJavaList(rsdw, diagnostics)
      }
    }
  }

  /**
   * Pool of registers for use by low-level text DFA code.
   */
  object dfaRegistersPool {
    private val pool = new RegistersPool()

    def getFromPool(requestorID: String) =
      pool.getFromPool(requestorID)

    def returnToPool(r: Registers) = pool.returnToPool(r)

    def finalCheck() = pool.finalCheck()
  }

  def toProcessingError(msg: String): ProcessingError = {
    val diagnostic = this match {
      case ps: PState =>
        new ParseError(
          rd = Maybe(ps.schemaFileLocation),
          loc = Maybe(ps.currentLocation),
          causedBy = Maybe.Nope,
          kind = Maybe(msg)
        )
      case us: UState =>
        new UnparseError(
          rd = Maybe(us.schemaFileLocation),
          loc = Maybe(us.currentLocation),
          causedBy = Maybe.Nope,
          kind = Maybe(msg)
        )
    }
    diagnostic
  }

  def toProcessingError(e: Throwable): ProcessingError = {
    val diagnostic = this match {
      case ps: PState =>
        new ParseError(
          rd = Maybe(ps.schemaFileLocation),
          loc = Maybe(ps.currentLocation),
          causedBy = Maybe(e),
          kind = Maybe.Nope
        )
      case us: UState =>
        new UnparseError(
          rd = Maybe(us.schemaFileLocation),
          loc = Maybe(us.currentLocation),
          causedBy = Maybe(e),
          kind = Maybe.Nope
        )
    }
    diagnostic
  }
}

/**
 * State used when compiling Evaluatable[T] objects
 *  So they don't require a "real" state.
 *
 *  This serves two purposes. First it lets us obey the regular API for evaluation, so we don't need
 *  one way to evaluate and another very similar thing for analyzing expressions to see if they are constant.
 *
 *  Second, it serves as a detector of when an expression is non-constant by blowing up when things
 *  inconsistent with constant-value are attempted to be extracted from the state. By "blow up" it throws
 *  a structured set of exceptions, typically children of InfosetException or VariableException.
 */
final class CompileState(
  tci: DPathCompileInfo,
  maybeDataProc: Maybe[DataProcessor],
  tunable: DaffodilTunables
) extends ParseOrUnparseState(tci.variableMap, Nil, maybeDataProc, tunable) {

  def arrayIterationPos: Long = 1L
  def occursPos: Long = 1L
  def bitLimit0b: MaybeULong = MaybeULong.Nope
  def bitPos0b: Long = 0L
  def childPos: Long = 0L
  def dataStream = Nope
  def groupPos: Long = 0L
  def hasInfoset: Boolean = infoset_.isDefined
  def delimitedParseResult = Nope
  def suspensions = Seq.empty

  private lazy val infoset_ : Maybe[DIElement] = Nope

  def infoset: DIElement =
    if (infoset_.isDefined)
      infoset_.value
    else
      throw new InfosetNoInfosetException(
        One(tci)
      ) // for expressions evaluated in debugger, default expressions for top-level variable decls.

  def currentNode = Maybe(infoset.asInstanceOf[DINode])

  def notifyDebugging(flag: Boolean): Unit = {
    // do nothing
  }
  private val occursBoundsStack_ = MStackOfLong()

  def thisElement = infoset

  // Members declared in org.apache.daffodil.runtime1.processors.StateForDebugger
  def currentLocation: DataLocation = Assert.usageError("Not to be used.")

  protected def checkBitOrder(): Unit = {
    // do nothing
  }

  def regexMatchBuffer: CharBuffer = Assert.usageError("Not to be used.")
  def regexMatchBitPositionBuffer: LongBuffer = Assert.usageError("Not to be used.")

  // $COVERAGE-OFF$
  override def setVariable(
    vrd: VariableRuntimeData,
    newValue: DataValuePrimitive,
    referringContext: ThrowsSDE
  ): Unit = Assert.usageError("Not to be used.")
  override def getVariable(vrd: VariableRuntimeData, referringContext: ThrowsSDE) =
    Assert.usageError("Not to be used.")
  override def newVariableInstance(vrd: VariableRuntimeData) =
    Assert.usageError("Not to be used.")
  override def removeVariableInstance(vrd: VariableRuntimeData): Unit =
    Assert.usageError("Not to be used.")
  // $COVERAGE-ON$
}
