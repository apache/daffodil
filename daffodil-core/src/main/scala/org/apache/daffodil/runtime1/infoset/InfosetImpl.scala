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
package org.apache.daffodil.runtime1.infoset

import java.lang.{ Boolean => JBoolean }
import java.lang.{ Byte => JByte }
import java.lang.{ Double => JDouble }
import java.lang.{ Float => JFloat }
import java.lang.{ Integer => JInt }
import java.lang.{ Long => JLong }
import java.lang.{ Number => JNumber }
import java.lang.{ Short => JShort }
import java.lang.{ String => JString }
import java.math.{ BigDecimal => JBigDecimal }
import java.math.{ BigInteger => JBigInt }
import java.net.URI
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable.ArrayBuffer

import org.apache.daffodil.api.ComplexElementMetadata
import org.apache.daffodil.api.ElementMetadata
import org.apache.daffodil.api.InfosetArray
import org.apache.daffodil.api.InfosetComplexElement
import org.apache.daffodil.api.InfosetDocument
import org.apache.daffodil.api.InfosetElement
import org.apache.daffodil.api.InfosetSimpleElement
import org.apache.daffodil.api.InfosetTypeException
import org.apache.daffodil.api.SimpleElementMetadata
import org.apache.daffodil.io.DataOutputStream
import org.apache.daffodil.io.DirectOrBufferedDataOutputStream
import org.apache.daffodil.lib.calendar.DFDLCalendar
import org.apache.daffodil.lib.equality.TypeEqual
import org.apache.daffodil.lib.equality.ViewEqual
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.ThinException
import org.apache.daffodil.lib.iapi.DaffodilTunables
import org.apache.daffodil.lib.iapi.Diagnostic
import org.apache.daffodil.lib.iapi.ThinDiagnostic
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.lib.util.MaybeBoolean
import org.apache.daffodil.lib.util.MaybeInt
import org.apache.daffodil.lib.util.MaybeULong
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.util.Numbers
import org.apache.daffodil.lib.xml.NamedQName
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.dsom.DPathCompileInfo
import org.apache.daffodil.runtime1.dsom.DPathElementCompileInfo
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitiveNullable
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.EvalCache
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.ProcessingError
import org.apache.daffodil.runtime1.processors.SimpleTypeRuntimeData
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.parsers.PState

import com.ibm.icu.util.Calendar
import passera.unsigned.ULong

sealed trait DINode {

  def diParent: DINode

  def asSimple: DISimple = {
    this match {
      case diSimple: DISimple => diSimple
      case _ => {
        erd.toss(
          new InfosetWrongNodeType("simpleType", this)
        ) // see comment with exception class definition for why this can happen
      }
    }
  }
  def isSimple: Boolean

  def asComplex: DIComplex = {
    this match {
      case diComplex: DIComplex => diComplex
      case _ => {
        erd.toss(
          new InfosetWrongNodeType("complexType", this)
        ) // see comment with exception class definition for why this can happen
      }
    }
  }
  def isComplex: Boolean

  def asArray: DIArray = {
    this match {
      case diArray: DIArray => diArray
      case _ => {
        erd.toss(
          new InfosetWrongNodeType("arrayType", this)
        ) // see comment with exception class definition for why this can happen
      }
    }
  }
  def isArray: Boolean

  def asElement: DIElement =
    if (isSimple) asSimple
    else asComplex

  def isDefaulted: Boolean
  def isHidden: Boolean

  def indexOf(item: DINode): Int
  def find(func: DINode => Boolean): Option[DINode]
  def namedQName: NamedQName
  def erd: ElementRuntimeData

  def infosetWalkerBlockCount: Int

  def child(index: Int): DINode
  def numChildren: Int

  /**
   * If there are any children, returns the last child as a One(lastChild).
   * Otherwise returns Nope. Note that because Daffodil may set children to
   * null when it determines they are no longer needed, and One(null) is not
   * allowed, this must return Nope if the last child has been freed. Thus,
   * this is not a reliable mechanism to determine if children exist. It should
   * only be used where we need to modify the state (e.g. isFinal) of the last
   * child IF it exists.
   */
  def maybeLastChild: Maybe[DINode]

  /**
   * Free memory associated with the child at a specified index. Note that this
   * should only free children if they are not actually needed anymore. It can
   * assume that the infoset events have been created, but if, for example, a
   * child is used in DPath expressions this should not free the child.
   *
   * For testing purposes, the doRelease argument says if it should actually
   * free the child. If false, this will not free the child and instead just
   * marks the wouldHaveBeenFreed variable.
   */
  def freeChildIfNoLongerNeeded(index: Int, doFree: Boolean): Unit

  var wouldHaveBeenFreed: Boolean = false

  /**
   * When unparsing, arrays and complex elements have a notion of being finalized.
   * This is when we know that no more elements will be added to them, so things
   * like fn:count can return a value knowing it won't change, and fn:exists can
   * return false, knowing nobody will subsequently append the item that was being
   * questioned.
   */
  var isFinal: Boolean = false

  /**
   * use to require it be finalized or throw the appropriate
   * Array or Complex exception.
   */
  def requireFinal(): Unit
}

/**
 * Marker trait that identifies exceptions which indicate an action that can block
 * forward-referencing expressions that occur during unparsing. These actions
 * can be retried once the available information has "arrived" in the infoset or
 * the state of the unparser.
 */
trait RetryableException

trait InfosetException {
  self: Diagnostic =>
  def asDiagnostic = self
}

/**
 * Indicates that an expression evaluated to a node sequence of more than
 * one value. This results in a schema definition error at runtime.
 */
case class InfosetAmbiguousNodeException(node: DIComplex, nqn: NamedQName)
  extends ThinDiagnostic(
    One(node.erd.schemaFileLocation),
    Nope,
    Nope,
    One(
      "Path step '%s' ambiguous. More than one infoset node corresponds to this name.\n" +
        "Query-style expressions are not supported."
    ),
    nqn.toExtendedSyntax
  )
  with InfosetException {
  def isError = true
  def modeName = "Schema Definition"
}

trait InfosetNodeNotFinalException extends InfosetException with RetryableException {
  self: Diagnostic =>
  def node: DINode
}

case class InfosetArrayNotFinalException(override val node: DIArray)
  extends ProcessingError("Expression Evaluation", Nope, Nope, "Array is not finalized.", node)
  with InfosetNodeNotFinalException

case class InfosetComplexElementNotFinalException(override val node: DIComplex)
  extends ProcessingError(
    "Expression Evaluation",
    Nope,
    Nope,
    "ComplexType element is not finalized.",
    node
  )
  with InfosetNodeNotFinalException

/**
 * This is thrown if an expression is evaluated at the wrong place
 * Eg., in the debugger if you set a breakpoint, and then
 *
 *     condition 1 xsd:string(.) eq '3'
 *
 * Well that condition expression is going to get evaluated even at times
 * when "." is bound to a DIComplex node. (The focus of "." seems to not
 * be assured of being on the breakpoint node. This expression can get evaluated
 * at least when "." is the parent of the breakpoint element.)
 *
 * These are all case classes so we get the automatic equals function that compares
 * the constructor args for equality.
 */
case class InfosetWrongNodeType(expectedType: String, val node: DINode)
  extends ProcessingError(
    "Expression Evaluation",
    Nope,
    Nope,
    "Expression expected %s to be a %s node.",
    node.namedQName,
    expectedType
  )
  with InfosetException

/**
 * Exception thrown if infoset doesn't have a child corresponding to the
 * slot being probed. E.g., expression evaluation reaching to a forward
 * sibling that has not yet been parsed.
 */
case class InfosetNoSuchChildElementException(val diComplex: DIComplex, nqn: NamedQName)
  extends ProcessingError(
    "Expression Evaluation",
    Nope,
    Nope,
    "Child element %s does not exist.",
    nqn
  )
  with InfosetException
  with RetryableException

case class InfosetNoNextSiblingException(
  val diSimple: DISimple,
  val info: DPathElementCompileInfo
) extends ProcessingError(
    "Expression Evaluation",
    Nope,
    Nope,
    "Element %s does not have a nextSibling",
    info.namedQName
  )
  with InfosetException
  with RetryableException

case class InfosetNoInfosetException(val rd: Maybe[DPathCompileInfo])
  extends ProcessingError(
    "Expression Evaluation",
    Nope,
    Nope,
    "There is no infoset%s",
    (if (rd.isEmpty) "." else " for path %s.".format(rd.get.path))
  )
  with InfosetException
  with RetryableException

/**
 * Indicates for simple types that there is no value, and has not been setNilled.
 *
 * For complex types indicates has not been setNilled.
 */
sealed abstract class InfosetNoDataExceptionBase(
  val diElement: DIElement,
  val erd: ElementRuntimeData,
  message: String
) extends ProcessingError(
    "Expression Evaluation",
    One(erd.schemaFileLocation),
    Nope,
    "%s %s",
    message,
    erd.namedQName
  )
  with InfosetException
  with RetryableException

case class InfosetNoDataException(
  override val diElement: DIElement,
  override val erd: ElementRuntimeData
) extends InfosetNoDataExceptionBase(diElement, erd, "Element does not have a value.")

case class InfosetSelfReferencingException(
  override val diElement: DIElement,
  override val erd: ElementRuntimeData
) extends InfosetNoDataExceptionBase(
    diElement,
    erd,
    "Self referencing element does not have a value."
  )

case class InfosetArrayIndexOutOfBoundsException(
  val diArray: DIArray,
  val index: Long,
  val length: Long
) extends ProcessingError(
    "Expression Evaluation",
    Nope,
    Nope,
    "Value %d is out of range for the '%s' array with length %d",
    index,
    diArray.erd.namedQName,
    length
  )
  with InfosetException
  with RetryableException

/**
 * Don't catch this one. It's not restartable.
 */
case class InfosetFatalArrayIndexOutOfBoundsException(
  val diArray: DIArray,
  val index: Long,
  val length: Long
) extends ProcessingError(
    "Expression Evaluation",
    Nope,
    Nope,
    "Value %d is out of range for the '%s' array with length %d",
    index,
    diArray.erd.namedQName,
    length
  )
  with InfosetException

case class InfosetNoRootException(val diElement: DIElement, val erd: ElementRuntimeData)
  extends ProcessingError(
    "Expression Evaluation",
    Nope,
    Nope,
    "No root element reachable from element %s.",
    erd.namedQName
  )
  with InfosetException
  with RetryableException

case class InfosetNoParentException(val diElement: DIElement, val erd: ElementRuntimeData)
  extends ProcessingError(
    "Expression Evaluation",
    Nope,
    Nope,
    "No parent element for element %s.",
    erd.namedQName
  )
  with InfosetException

sealed abstract class InfosetLengthUnknownException(
  lengthState: LengthState,
  kind: String,
  val diElement: DIElement,
  val erd: ElementRuntimeData
) extends ProcessingError(
    "Expression Evaluation",
    Nope,
    Nope,
    "%s length unknown for element '%s'.",
    kind,
    erd.namedQName
  )
  with InfosetException
  with RetryableException

case class InfosetContentLengthUnknownException(
  lengthState: LengthState,
  override val diElement: DIElement,
  override val erd: ElementRuntimeData
) extends InfosetLengthUnknownException(lengthState, "Content", diElement, erd)

case class InfosetValueLengthUnknownException(
  lengthState: LengthState,
  override val diElement: DIElement,
  override val erd: ElementRuntimeData
) extends InfosetLengthUnknownException(lengthState, "Value", diElement, erd)

case class InfosetMultipleScalarError(val erd: ElementRuntimeData)
  extends ProcessingError(
    "Scalar Element",
    Nope,
    Nope,
    "Multiple instances detected for scalar element %s",
    erd.namedQName
  )

/**
 * Used to determine if expressions can be evaluated without any nodes.
 * They all save/restore the current node, so this is the placeholder
 * they use for that purpose.
 */
final class FakeDINode extends DISimple(null) {
  override def dataValue: DataValuePrimitiveNullable = _value
  override def setDataValue(s: DataValuePrimitiveNullable): Unit = { _value = s }
  override def dataValueAsString: String = _value.toString
  // $COVERAGE-OFF$
  private def die = throw new InfosetNoInfosetException(Nope)
  override def parent = die
  override def diParent = die
  override def setParent(p: DIComplex): Unit = die
  override def isNilled: Boolean = die
  override def setNilled(): Unit = die
  override def valid = die
  override def setValid(validity: Boolean): Unit = die
  override def isDefaulted: Boolean = die
  override def contentLength: ContentLengthState = die
  override def valueLength: ValueLengthState = die
  override def primType: NodeInfo.PrimType = die
  override def getNonNegativeInteger: JBigInt = die
  // $COVERAGE-ON$
}

/**
 * Base for all terms, as there are these pseudo-nodes like DISequence which can have
 * runtime-valued properties, but which aren't infoset items.
 */
sealed trait DITerm {

  final lazy val parserEvalCache = new EvalCache
  final lazy val unparserEvalCache = new EvalCache

  def evalCache(state: ParseOrUnparseState): EvalCache = state match {
    case p: PState => parserEvalCache
    case _ => unparserEvalCache
  }

  def trd: TermRuntimeData
  final def termRuntimeData: TermRuntimeData = trd
}

/**
 * Keeps track of where we are for computing either the value length or content length.
 * Serves to cache this information on the infoset element.
 *
 * Note that this is used to capture and restore this state also, in which case
 * the ie argument is null.
 *
 * These  4 members are used to allow computation of e.g., contentLength but in a manner
 * than can block if it is unable to be computed yet.
 *
 * If only the maybeStartPos... is defined, AND the corresponding data output stream is NOT defined
 * then it is an absolute position.
 * If both the maybeStartPos... is defined AND the corresponding maybeStartDataOutputStream
 * are defined, then that start pos is a relative start pos, and we'll need to compute the
 * absolute start pos once we find out the absolute start pos of the data output stream.
 */
sealed abstract class LengthState(ie: DIElement) {

  protected def flavor: String

  override def toString() = {
    lazy val stDOSid =
      if (maybeStartDataOutputStream.isDefined)
        "DOS(id=%s)".format(maybeStartDataOutputStream.get.id.toString)
      else
        "unk"
    lazy val eDOSid =
      if (maybeEndDataOutputStream.isDefined)
        "DOS(id=%s)".format(maybeEndDataOutputStream.get.id.toString)
      else
        "unk"
    try {
      val (stStatus, stAbs, stDOS) =
        if (maybeStartPos0bInBits.isEmpty) ("unk", "", "")
        else ("", maybeStartPos0bInBits, stDOSid)

      val (eStatus, eAbs, eDOS) =
        if (maybeEndPos0bInBits.isEmpty) ("unk", "", "")
        else ("", maybeEndPos0bInBits, eDOSid)

      "%s(%s%s%s, %s%s%s)".format(flavor, stStatus, stAbs, stDOS, eStatus, eAbs, eDOS)
    } catch {
      case th: Throwable => {
        val res = flavor + "(" + hashCode + ")"
        res
      }
    }
  }

  var maybeStartDataOutputStream: Maybe[DataOutputStream] = Nope
  var maybeStartPos0bInBits: MaybeULong = MaybeULong.Nope
  var maybeEndDataOutputStream: Maybe[DataOutputStream] = Nope
  var maybeEndPos0bInBits: MaybeULong = MaybeULong.Nope
  var maybeComputedLengthInBits: MaybeULong = MaybeULong.Nope

  def copyFrom(other: LengthState): Unit = {
    this.maybeStartDataOutputStream = other.maybeStartDataOutputStream
    this.maybeStartPos0bInBits = other.maybeStartPos0bInBits
    this.maybeEndDataOutputStream = other.maybeEndDataOutputStream
    this.maybeEndPos0bInBits = other.maybeEndPos0bInBits
    this.maybeComputedLengthInBits = other.maybeComputedLengthInBits
  }

  def clear(): Unit = {
    maybeStartDataOutputStream = Nope
    maybeStartPos0bInBits = MaybeULong.Nope
    maybeEndDataOutputStream = Nope
    maybeEndPos0bInBits = MaybeULong.Nope
    maybeComputedLengthInBits = MaybeULong.Nope
  }

  protected def throwUnknown: Nothing

  /**
   * Examines the underlying streams (if they exist) where the start and end
   * of the element were determined to be. If those have taken on absolute
   * position information, then we bring that up here so that it can be used
   * to compute the value or content length.
   */
  private def recheckStreams(): Unit = {
    if (maybeStartDataOutputStream.isDefined) {
      Assert.invariant(maybeStartPos0bInBits.isDefined)
      val dos =
        this.maybeStartDataOutputStream.get.asInstanceOf[DirectOrBufferedDataOutputStream]
      if (dos.maybeAbsBitPos0b.isDefined) {
        // we don't have absolute bit positions in this infoset element,
        // but the stream now has that information, so we have to migrate
        // that here.
        val thisRelStartPos0bInBits = this.maybeStartPos0bInBits.getULong
        val newStartBitPos0b = dos.toAbsolute(thisRelStartPos0bInBits)
        Logger.log.debug(
          s"${flavor}gth for ${ie.name} new absolute start pos: ${newStartBitPos0b}"
        )
        this.maybeStartDataOutputStream = Nope
        this.maybeStartPos0bInBits = MaybeULong(newStartBitPos0b.longValue)
      }
    }
    if (maybeEndDataOutputStream.isDefined) {
      Assert.invariant(maybeEndPos0bInBits.isDefined)
      val dos = this.maybeEndDataOutputStream.get.asInstanceOf[DirectOrBufferedDataOutputStream]
      if (dos.maybeAbsBitPos0b.isDefined) {
        // we don't have absolute bit positions in this infoset element,
        // but the stream now has that information, so we have to migrate
        // that here.
        val thisRelEndPos0bInBits = this.maybeEndPos0bInBits.getULong
        val newEndBitPos0b = dos.toAbsolute(thisRelEndPos0bInBits)
        Logger.log.debug(s"${flavor}gth for ${ie.name} new absolute end pos: ${newEndBitPos0b}")
        this.maybeEndDataOutputStream = Nope
        this.maybeEndPos0bInBits = MaybeULong(newEndBitPos0b.longValue)
      }
    }
  }

  def isStartUndef = {
    val r = maybeStartPos0bInBits.isEmpty
    if (r) Assert.invariant(maybeStartDataOutputStream.isEmpty)
    r
  }

  def isEndUndef = {
    val r = maybeEndPos0bInBits.isEmpty
    if (r) Assert.invariant(maybeEndDataOutputStream.isEmpty)
    r
  }

  def isStartAbsolute = {
    maybeStartPos0bInBits.isDefined &&
    maybeStartDataOutputStream.isEmpty
  }

  def isEndAbsolute = {
    maybeEndPos0bInBits.isDefined &&
    maybeEndDataOutputStream.isEmpty
  }

  def isStartRelative = {
    val res = maybeStartDataOutputStream.isDefined
    if (res) {
      Assert.invariant(maybeStartPos0bInBits.isDefined)
    }
    res
  }

  def isEndRelative = {
    val res = maybeEndDataOutputStream.isDefined
    if (res) {
      Assert.invariant(maybeEndPos0bInBits.isDefined)
    }
    res
  }

  /**
   * This returns Nope when the length is unable to be computed currently.
   *
   * (Note that this differs from the way the length-related Evs work where
   * returning Nope means variable-width encoding and lengthUnits characters
   * were being used, and inability to compute is indicated by thrown exceptions.)
   */
  def maybeLengthInBits(): MaybeULong = {
    recheckStreams()
    val computed: MaybeULong = {
      if (maybeComputedLengthInBits.isDefined) {
        val len = maybeComputedLengthInBits.get
        Logger.log.debug(s"${flavor}gth of ${ie.name} is ${len}, (was already computed)")
        return maybeComputedLengthInBits
      } else if (isStartUndef || isEndUndef) {
        Logger.log.debug(s"${flavor}gth of ${ie.name} cannot be computed yet. ${toString}")
        MaybeULong.Nope
      } else if (isStartAbsolute && isEndAbsolute) {
        val len = maybeEndPos0bInBits.get - maybeStartPos0bInBits.get
        Logger.log.debug(
          s"${flavor}gth of ${ie.name} is ${len}, by absolute positions. ${toString}"
        )
        MaybeULong(len)
      } else if (
        isStartRelative && isEndRelative && (maybeStartDataOutputStream.get._eq_(
          maybeEndDataOutputStream.get
        ))
      ) {
        //
        // they're the same DOS, so we have a relative distance between two
        // points in the same buffering stream.
        // Usually we think of this happening for simple types, but
        // there's no reason it cannot also happen for complex types much of
        // the time if there isn't anything wierd like interior alignment.
        //
        Assert.invariant(maybeStartPos0bInBits.isDefined)
        val startPos = maybeStartPos0bInBits.get
        val endPos = maybeEndPos0bInBits.get
        val len = endPos - startPos
        Logger.log.debug(
          s"${flavor}gth of ${ie.name} is ${len}, by relative positions in same data stream. ${toString}"
        )
        MaybeULong(len)
      } else if (
        isStartRelative && isEndRelative && maybeStartDataOutputStream.get.isFinished
      ) {
        // if start and end DOSs are relative and different, but every DOS from
        // start inclusive to end excluste is finished, then we can calculate
        // the length based on relative positions. Note that it is fine for the
        // end DOS to be active because all we need are the relative bit
        // positive from the beginning of the DOS. We don't care about any bits
        // that maybe or may not occurr after it.

        // get partial lengths in the start and end DOS
        var len =
          (maybeStartDataOutputStream.get.relBitPos0b - maybeStartPos0bInBits.getULong) + maybeEndPos0bInBits.getULong

        // get all bits from DOS between start and end DOSs
        var dos =
          maybeEndDataOutputStream.get.asInstanceOf[DirectOrBufferedDataOutputStream].splitFrom
        val stop = maybeStartDataOutputStream.get.asInstanceOf[DirectOrBufferedDataOutputStream]
        while (dos.isFinished && (dos._ne_(stop))) {
          len = len + dos.relBitPos0b
          dos = dos.splitFrom
        }

        if (!dos.isFinished) {
          // found a non-finished DOS, can't calculate length
          Logger.log.debug(
            s"${flavor}gth of ${ie.name} is unknown due to unfinished output stream. ${toString}"
          )
          MaybeULong.Nope
        } else {
          Logger.log.debug(
            s"${flavor}gth of ${ie.name} is ${len}, by relative positions in same data stream. ${toString}"
          )
          MaybeULong(len.toLong)
        }
      } else {
        Logger.log.debug(s"${flavor}gth of ${ie.name} is unknown still. ${toString}")
        MaybeULong.Nope
      }
    }
    maybeComputedLengthInBits = computed
    computed
  }

  def lengthInBits: ULong = {
    val mlib = maybeLengthInBits()
    if (mlib.isDefined) mlib.getULong
    else throwUnknown
  }

  def maybeLengthInBytes(): MaybeULong = {
    // TODO: assert check for length in bytes makes sense - length units is bytes, or is characters, but characters
    // are byte based (that is not 7-bit or 6-bit)
    // or is bits, but for some reason we know it's a multiple of 8.
    // For now, this is just going to round up to the next number of bytes whenever there is a frag byte.
    val mlib = maybeLengthInBits()
    if (mlib.isDefined) {
      val lbits = mlib.get
      val wholeBytes = math.ceil(lbits.toDouble / 8.0).toLong
      MaybeULong(wholeBytes)
    } else {
      MaybeULong.Nope
    }
  }

  def lengthInBytes: ULong = {
    val mlib = maybeLengthInBytes()
    if (mlib.isDefined) mlib.getULong
    else throwUnknown
  }

  def setAbsStartPos0bInBits(absPosInBits0b: ULong): Unit = {
    Assert.invariant(
      maybeStartPos0bInBits.isEmpty,
      s"maybeStartPos0bInBits already has a value: $maybeStartPos0bInBits"
    )
    maybeStartPos0bInBits = MaybeULong(absPosInBits0b.longValue)
    maybeStartDataOutputStream = Nope
  }

  def setRelStartPos0bInBits(relPosInBits0b: ULong, dos: DataOutputStream): Unit = {
    maybeStartPos0bInBits = MaybeULong(relPosInBits0b.longValue)
    maybeStartDataOutputStream = One(dos)
  }

  def setAbsEndPos0bInBits(absPosInBits0b: ULong): Unit = {
    Assert.invariant(
      maybeEndPos0bInBits.isEmpty,
      s"maybeEndPos0bInBits already has a value: $maybeEndPos0bInBits"
    )
    maybeEndPos0bInBits = MaybeULong(absPosInBits0b.longValue)
    maybeEndDataOutputStream = Nope
  }

  def setRelEndPos0bInBits(relPosInBits0b: ULong, dos: DataOutputStream): Unit = {
    maybeEndPos0bInBits = MaybeULong(relPosInBits0b.longValue)
    maybeEndDataOutputStream = One(dos)
  }

  /**
   * returns the earliest unfinished preceding DOS
   * or finished, but without absolute positioning information.
   */
  def diagnoseNoLength(): String = {
    if (isStartRelative) {
      val s = this.maybeStartDataOutputStream.get.asInstanceOf[DirectOrBufferedDataOutputStream]
      s.findFirstBlocking.toString
    } else if (isEndRelative) {
      val s = this.maybeEndDataOutputStream.get.asInstanceOf[DirectOrBufferedDataOutputStream]
      s.findFirstBlocking.toString
    } else if (isStartUndef) {
      toString
    } else if (isEndUndef) {
      toString
    } else
      Assert.invariantFailed("absolute streams cannot block length calculations: " + this)
  }
}

class ContentLengthState(ie: DIElement) extends LengthState(ie) {

  override def flavor = "contentLen"

  override def throwUnknown = {
    Assert.invariant(ie ne null)
    ie.erd.toss(new InfosetContentLengthUnknownException(this, ie, ie.runtimeData))
  }

}

class ValueLengthState(ie: DIElement) extends LengthState(ie) {

  override def flavor = "valueLen"

  override def throwUnknown = {
    Assert.invariant(ie ne null)
    ie.erd.toss(new InfosetValueLengthUnknownException(this, ie, ie.runtimeData))
  }

}

sealed trait DIElementSharedInterface {
  def contentLength: ContentLengthState
  def valueLength: ValueLengthState
  def restoreInto(e: DIElement): Unit
  def captureFrom(e: DIElement): Unit
  def clear(): Unit
}

sealed trait DIElementSharedMembersMixin {

  protected final var _contentLength: ContentLengthState = null
  protected final var _valueLength: ValueLengthState = null

  /**
   * Default isNilled to false. Note that this is safe since this field cannot be accessed in
   * the time between an element being created and the determination that an element is nil by a
   * NilParser or an InfosetInputter. The only things that are evaluated during this time are
   * things like discriminators/setVar, but the kinds that can evaluate expression that might
   * access isNilledState are evaluated at the end of the element, well after it is determined
   * to be nilled or not
   */
  protected final var _isNilled: Boolean = false
  protected final var _validity: MaybeBoolean = MaybeBoolean.Nope

  protected def allocContentLength: ContentLengthState
  protected def allocValueLength: ValueLengthState

  /**
   * Copy method keeps these objects null
   * to avoid allocation unless they are really needed.
   */
  final protected def copyContentLengthFrom(e: DIElementSharedMembersMixin): Unit = {
    if (e._contentLength eq null)
      if (this._contentLength eq null) {
        // do nothing
      } else {
        this._contentLength = null
      }
    else {
      if (this._contentLength eq null) {
        this._contentLength = allocContentLength
      }
      this._contentLength.copyFrom(e._contentLength)
    }
  }

  /**
   * Copy method keeps these objects null
   * to avoid allocation unless they are really needed.
   */
  final protected def copyValueLengthFrom(e: DIElementSharedMembersMixin): Unit = {
    if (e._valueLength eq null)
      if (this._valueLength eq null) {
        // do nothing
      } else {
        this._valueLength = null
      }
    else {
      if (this._valueLength eq null) {
        this._valueLength = allocValueLength
      }
      this._valueLength.copyFrom(e._valueLength)
    }
  }

  /*
   * Initialized on demand
   */
  protected final def clearContentLength() = {
    if (_contentLength eq null) ()
    else _contentLength.clear()
  }

  def contentLength = {
    if (_contentLength eq null) {
      _contentLength = allocContentLength
    }
    _contentLength
  }

  protected final def clearValueLength() = {
    if (_valueLength eq null) ()
    else _valueLength.clear()
  }

  def valueLength = {
    if (_valueLength eq null) {
      _valueLength = allocValueLength
    }
    _valueLength
  }

}

sealed abstract class DIElementState
  extends DIElementSharedInterface
  with DIElementSharedImplMixin {

  final override protected def allocContentLength = new ContentLengthState(null)
  final override protected def allocValueLength = new ValueLengthState(null)

}

sealed trait DIElementSharedImplMixin
  extends DIElementSharedInterface
  with DIElementSharedMembersMixin {

  override def restoreInto(e: DIElement): Unit = {
    e._isNilled = this._isNilled
    e._validity = this._validity

    e.copyContentLengthFrom(this)
    e.copyValueLengthFrom(this)
  }

  override def captureFrom(e: DIElement): Unit = {
    this._isNilled = e._isNilled
    this._validity = e._validity

    this.copyContentLengthFrom(e)
    this.copyValueLengthFrom(e)
  }

  override def clear(): Unit = {
    this._isNilled = false
    this._validity = MaybeBoolean.Nope
    clearContentLength()
    clearValueLength()
  }

}

sealed trait DIComplexSharedMembersMixin {

  final var _numChildren: Int = 0
  final var _arraySize: MaybeInt = MaybeInt.Nope
}

sealed trait DIComplexSharedImplMixin
  extends DIElementSharedInterface
  with DIComplexSharedMembersMixin {

  abstract override def restoreInto(e: DIElement): Unit = {
    val c = e.asInstanceOf[DIComplex]
    c.restoreFrom(this)
    c._numChildren = this._numChildren
    super.restoreInto(e)
  }

  abstract override def captureFrom(e: DIElement): Unit = {
    val c = e.asInstanceOf[DIComplex]
    c.captureInto(this)
    this._numChildren = c._numChildren
    super.captureFrom(e)
  }

  abstract override def clear(): Unit = {
    _numChildren = 0
    _arraySize = MaybeInt.Nope
    super.clear()
  }
}

final class DIComplexState private ()
  extends DIElementState
  with DIElementSharedImplMixin
  with DIComplexSharedImplMixin

object DIComplexState {
  def apply() = { val c = new DIComplexState(); c.clear(); c }
}

sealed trait DISimpleSharedMembersMixin {

  protected var _isDefaulted: Boolean = false

  protected var _value: DataValuePrimitiveNullable = DataValue.NoValue

  protected var _unionMemberRuntimeData: Maybe[SimpleTypeRuntimeData] = Nope
}

sealed trait DISimpleSharedImplMixin
  extends DIElementSharedInterface
  with DISimpleSharedMembersMixin {

  abstract override def restoreInto(e: DIElement): Unit = {
    super.restoreInto(e)
    val s = e.asInstanceOf[DISimple]
    s._isDefaulted = this._isDefaulted
    s._value = this._value
    s._unionMemberRuntimeData = this._unionMemberRuntimeData
  }

  abstract override def captureFrom(e: DIElement): Unit = {
    super.captureFrom(e)
    val s = e.asInstanceOf[DISimple]
    this._isDefaulted = s._isDefaulted
    this._value = s._value
    this._unionMemberRuntimeData = s._unionMemberRuntimeData
  }

  abstract override def clear(): Unit = {
    super.clear()
    _isDefaulted = false
    _value = DataValue.NoValue
    _unionMemberRuntimeData = Nope
  }
}

final class DISimpleState private ()
  extends DIElementState
  with DIElementSharedImplMixin
  with DISimpleSharedImplMixin

object DISimpleState {
  def apply() = { val s = new DISimpleState(); s.clear(); s }
}

/**
 * Base for non-array elements. That is either scalar or optional (
 * minOccurs 0, maxOccurs 1)
 */
sealed trait DIElement
  extends DINode
  with DITerm
  with DIElementSharedImplMixin
  with InfosetElement {

  final override protected def allocContentLength = new ContentLengthState(this)
  final override protected def allocValueLength = new ValueLengthState(this)

  def isSimple: Boolean
  def isComplex: Boolean
  def isArray: Boolean

  final def name: String = erd.name
  override final def namedQName: NamedQName = erd.namedQName
  override final def trd: TermRuntimeData = erd

  /**
   * Used to prevent the infoset walker from walking into an infoset element.
   * This is generally incremented when marks are added and decremented when
   * they are discarded. The idea being that we do not want to walk into
   * infoset elements if there could be backtracking. Though this is not
   * strictly the only time blocks may be added. For example, we also increment
   * this when in unordered sequences to prevent walking until elements are
   * reordered to schema definition order.
   */
  var infosetWalkerBlockCount: Int = 0

  /**
   * Tells us if the element was entirely created by defaulting elements.
   */
  def isDefaulted: Boolean

  /**
   * This is purely to make debugging easier.
   */
  override def toString = {
    val cl = Misc.getNameFromClass(this)
    val n = erd.name
    val clStr = if (_contentLength eq null) "" else " " + contentLength.toString()
    val vlStr = if (_valueLength eq null) "" else " " + valueLength.toString()
    val validStr =
      if (_validity.isEmpty) ""
      else if (_validity.get) " valid"
      else " not valid"
    val valStr = {
      val vsfd = valueStringForDebug
      if (vsfd == "") "" else " " + vsfd
    }
    cl + "(name='" + n + "'" + valStr + clStr + vlStr + validStr + ")"
  }

  def valueStringForDebug: String

  def isRoot: Boolean = toParent match {
    case doc: DIDocument => !doc.isCompileExprFalseRoot
    case _ => false
  }

  def isRootDoc = this match {
    case doc: DIDocument => !doc.isCompileExprFalseRoot
    case _ => false
  }

  final def toRootDoc: DIDocument = {
    if (isRootDoc) this.asInstanceOf[DIDocument]
    else toParent.toRootDoc
  }

  def toParent = {
    if (parent eq null)
      erd.toss(new InfosetNoParentException(this, erd))
    diParent
  }

  /**
   * If we are in a hidden context, isHidden should be true for the infoset
   * data member. Therefore, when a new element is created in a hidden context
   * we set isHidden to true.
   */
  protected final var _isHidden = false
  final def isHidden: Boolean = _isHidden

  def setHidden(): Unit = {
    _isHidden = true
  }

  final def runtimeData: ElementRuntimeData = erd
  protected final var _parent: DIComplex = null

  def parent: DIComplex = _parent

  def diParent: DIComplex = _parent.asInstanceOf[DIComplex]

  def setParent(p: DIComplex): Unit = {
    Assert.invariant(_parent eq null)
    _parent = p
  }

  private var _array: Maybe[DIArray] = Nope
  def maybeArray: Maybe[DIArray] = _array
  def setArray(a: DIArray): Unit = {
    _array = One(a)
  }

  /**
   * Tells if the element is nilled or not. For non-nillable elements this always returns false.
   */
  def isNilled: Boolean = {
    if (!erd.isNillable) false
    else _isNilled
  }

  def setNilled(): Unit = {
    Assert.invariant(erd.isNillable)
    _isNilled = true
  }

  /**
   * valid = Nope means not checked
   * valid = One(true) means valid
   * valid = One(false) means invalid
   */
  def valid: MaybeBoolean = _validity
  def setValid(validity: Boolean): Unit = { _validity = MaybeBoolean(validity) }

}

// This is not a mutable collection class on purpose.
// This forces use of while-loops and similar known-efficient
// code, rather than letting all sorts of map/flatmap compositions,
// which may or may not be optimized effectively.
//
final class DIArray(
  override val erd: ElementRuntimeData,
  val parent: DIComplex,
  initialSize: Int // TODO: really this needs to be adaptive, and resize upwards reasonably.
  // A non-copying thing - list like, may be better, but we do need access to be
  // constant time.
  // FIXME: for streaming behavior, arrays are going to get elements removed from
  // them when no longer needed. However, the array itself would still be growing
  // without bound. So, replace this with a mutable map so that it can shrink
  // as well as grow. // not saved. Needed only to get initial size.
) extends DINode
  with InfosetArray {

  override def metadata: ElementMetadata = erd

  private lazy val nfe = new InfosetArrayNotFinalException(this)

  override def requireFinal(): Unit = {
    if (!isFinal) throw nfe
  }

  override def isSimple = false
  override def isComplex = false
  override def isArray = true

  override def diParent = parent

  // Parsers don't actually call setHidden on DIArrays, only on DIElements. But
  // DIArrays are always created when there is at least one child element, and
  // since all children of an array must have the same visibility, we can just
  // inspect the isHidden property of the first child to determine if the
  // entire array is hidden.
  override def isHidden = _contents(0).isHidden

  // Marks/blocks are never taken on arrays, so the infosetWalkerBlockCount
  // would normally be zero for arrays. The infosetWalkerBlockCount is used to determine
  // if this array is known to exist or if it could be backtracked. We know
  // this DIArray exists if the first child is known to exist (i.e. if it has
  // no blocks).
  //
  // Additionally, the infoset walker may delete elements from an array. In
  // which case _contents(0) may be null. But if _contents(0) is null, that
  // means there was an element that was known to exist, and it was freed after
  // its event were created. So that doesn't cause a block.
  //
  // We also add the parent block count. This is because a mark is taken when
  // speculatively parsing a single element of an array. This mark increments
  // the infosetWalkerBlockCount of the /parent/ of the DIArray because the new
  // DIElement in this DIArray does not yet exist. So as long as we are
  // speculatively parsing a single element of an array, this block on the
  // parent prevents walking into this new speculatively parsed array element.
  // Once we finish speculatively parsing that single array element, that block
  // is removed, and we immediately call walk() to walk the new array element
  // (assuming there are no other PoU's in scope). This process is repeated for
  // each speculatively parsed array element.
  override def infosetWalkerBlockCount =
    parent.infosetWalkerBlockCount +
      (if (_contents(0) == null) 0 else _contents(0).infosetWalkerBlockCount)

  override def toString = "DIArray(" + namedQName + "," + _contents + ")"

  def namedQName = erd.namedQName

  protected final val _contents = new ArrayBuffer[DIElement](initialSize)

  override def indexOf(item: DINode): Int = _contents.indexOf(item)

  /**
   * Used to shorten array when backtracking out of having appended elements.
   */
  def reduceToSize(n: Int): Unit = {
    _contents.dropRightInPlace(_contents.length - n)
  }

  override def numChildren: Int = _contents.length

  override def child(index: Int): DINode = _contents(index)

  override def find(func: DINode => Boolean): Option[DINode] = _contents.find(func)

  override def maybeLastChild: Maybe[DINode] = {
    val len = _contents.length
    if (len > 0) {
      // note that if _contents(len - 1) has been removed (i.e. set to null)
      // then this will by Maybe(null) which is a Nope. This is expected
      // behavior
      Maybe(_contents(len - 1))
    } else Nope
  }

  /**
   * Access an item of the array.
   * Note that occursIndex argument starts at position 1.
   */
  def apply(occursIndex1b: Long): DIElement = {
    if (occursIndex1b < 1)
      erd.toss(
        new InfosetFatalArrayIndexOutOfBoundsException(this, occursIndex1b, length)
      ) // no blocking.
    if (occursIndex1b > length)
      erd.toss(
        new InfosetArrayIndexOutOfBoundsException(this, occursIndex1b, length)
      ) // can be retried after blocking.
    _contents(occursIndex1b.toInt - 1)
  }

  def append(ie: DIElement): Unit = {
    _contents += ie
    ie.setArray(this)
  }

  def concat(array: DIArray): Unit = {
    for (i <- 0 until array.numChildren) {
      val in = array.child(i)
      val ie = in.asInstanceOf[DIElement]
      ie.setArray(this)
      append(ie)
    }
  }

  final def length: Long = _contents.length

  final def isDefaulted: Boolean = _contents.forall { _.isDefaulted }

  final def freeChildIfNoLongerNeeded(index: Int, doFree: Boolean): Unit = {
    val node = _contents(index)
    if (!node.erd.dpathElementCompileInfo.isReferencedByExpressions) {
      if (doFree) {
        // set to null so that the garbage collector can free this node
        _contents(index) = null
      } else {
        node.wouldHaveBeenFreed = true
      }
    }
  }
}

/**
 * Thrown when unparsing and demanding the dataValue of a DISimple
 * when there is no value yet, but the element has dfdl:outputValueCalc
 * expression.
 *
 * This should be caught in contexts that want to undertake on-demand
 * evaluation of the OVC expression.
 */
case class OutputValueCalcEvaluationException(cause: Exception) extends ThinException(cause)

sealed class DISimple(override val erd: ElementRuntimeData)
  extends DIElement
  with DISimpleSharedImplMixin
  with InfosetSimpleElement {

  override def metadata: SimpleElementMetadata = erd

  final override def isSimple = true

  final override def isComplex = false

  final override def isArray = false

  def primType: NodeInfo.PrimType = erd.optPrimType.orNull

  private var _stringRep: String = null
  private var _bdRep: JBigDecimal = null

  override def child(index: Int): DINode =
    Assert.invariantFailed("DISimple cannot have child nodes")

  override def indexOf(item: DINode): Int = -1

  override def find(func: DINode => Boolean): Option[DINode] = None

  override def numChildren: Int = 0

  override def maybeLastChild: Maybe[DINode] = Nope

  def setDefaultedDataValue(defaultedValue: DataValuePrimitive) = {
    setDataValue(defaultedValue)
    _isDefaulted = true
  }

  def unionMemberRuntimeData = _unionMemberRuntimeData

  def setUnionMemberRuntimeData(umrd: SimpleTypeRuntimeData): Unit = {
    _unionMemberRuntimeData = Maybe(umrd)
    this.setValid(true)
  }

  /**
   * Parsing of a text number first does setDataValue to a string, then a conversion does overwrite data value
   * with a number. Unparsing does setDataValue to a value, then overwriteDataValue to a string.
   */
  def setDataValue(x: DataValuePrimitiveNullable): Unit = {
    Assert.invariant(!hasValue)
    overwriteDataValue(x)
  }

  def overwriteDataValue(x: DataValuePrimitiveNullable): Unit = {
    //
    // let's find places where we're putting a string in the infoset
    // but the simple type is not string. That happens when parsing or unparsing text Numbers, text booleans, text Date/Times.
    //
    //
    // There are 4 states to consider given _value and _stringRep
    //  _value null, _stringRep null
    //  _value not null, _stringRep null
    //  _value null, _stringRep not null
    //  both not null.
    //
    // Those go with parse x unparse, and whether the type of the DISimple node is String or not.
    //
    // when parsing or unparsing, if the incoming x is a string, then there are two cases
    // If the type of the DISimple element is String, then there is no distinction between textual _stringRep,
    // and _value.
    // If the type of the DISimple element is not String, then the incoming string is the textual data representation of
    // the value. The represetnation is a string for a textNumber, textBoolean, or textCalendar type.

    // If the incoming x is not a string, then _stringRep is only used as a cache of the text logical representation, as
    // for converting to XML, or for debugging print statements.
    //
    val nodeKind = erd.optPrimType.get
    val y = x.getAnyRef
    x.getAnyRef match {
      case xs: String => {
        if (nodeKind.isInstanceOf[NodeInfo.String.Kind]) {
          // the value is a string, and the type of the node is string.
          // so we set the value and stringRep to the same thing.
          _value = x
          _stringRep = xs
        } else {
          //
          // A string is being passed, but the type of the element is NOT string
          //
          // This is normal behavior for text numbers. First we parse the string
          // based on length rules and/or delimiters
          // then we run a quasi-parser which takes the value and converts
          // it by parsing it as a textual number. It then overwrites
          // the value with the value of the number type.
          //
          // So we set the stringRep, but not the value. (The value might be there if unparsing, or not if parsing)
          // When the converter runs, it will ask for the dataValueAsString which will take
          // the stringRep because we have defined it here.
          _stringRep = xs
        }
      }
      case _ => {
        // we've been given a non-string value.
        //
        Assert.invariant(!nodeKind.isInstanceOf[NodeInfo.String.Kind])
        //
        // In this case we set the stringRep to null. If we need the dataValueAsString, then it is NOT
        // to get a text physical representation in the data, it is to get a textual representation of the
        // logical value for debug or for XML output.
        //
        _stringRep = null
        _bdRep = null
        _value = x.getAnyRef match {
          case dc: DFDLCalendar => dc
          case arb: Array[Byte] => arb
          case b: JBoolean => b
          case _: AtomicLong | _: AtomicInteger =>
            Assert.invariantFailed(
              "Unsupported type. %s of type %s.".format(x, Misc.getNameFromClass(x))
            )
          case n: java.lang.Number => n
          case u: java.net.URI => u
          case ar: AnyRef =>
            Assert.invariantFailed(
              "Unsupported type. %s of type %s.".format(x, Misc.getNameFromClass(x))
            )
        }
      }
    }
    _isDefaulted = false
    _validity = MaybeBoolean.Nope // we have not tested this new value.
    _unionMemberRuntimeData = Nope
  }

  def resetValue(): Unit = {
    _isDefaulted = false
    _validity = MaybeBoolean.Nope // we have not tested this new value.
    _unionMemberRuntimeData = Nope
    _stringRep = null
    _bdRep = null
    _value = DataValue.NoValue
  }

  /**
   * This has to produce a useful string regardless of the
   * state of the node.
   */
  override def valueStringForDebug: String = {
    val res =
      if (_isNilled) "nil"
      else if (_value.isDefined) _value.toString()
      else if (_stringRep ne null) "stringRep(" + _stringRep + ")"
      else ""
    res
  }

  def hasValue: Boolean = !_isNilled && _value.isDefined

  /**
   * Obtain the data value. Implements default
   * values, and outputValueCalc for unparsing.
   */
  def dataValue: DataValuePrimitiveNullable = {
    if (_value.isEmpty)
      if (erd.optDefaultValue.isDefined) {
        val defaultVal = erd.optDefaultValue
        if (defaultVal == DataValue.UseNilForDefault) {
          this.setNilled()
        } else {
          _value = defaultVal.getNullablePrimitive
        }
        _isDefaulted = true
      } else {
        erd.toss(new InfosetNoDataException(this, erd))
      }
    if (_value.isEmpty) {
      this.erd.schemaDefinitionError("Value has not been set.")
    }
    _value.getNonNullable
  }

  final def maybeDataValue: DataValuePrimitiveNullable = {
    val mv =
      if (_value.isDefined)
        _value
      else if (erd.optDefaultValue.isDefined) {
        val defaultVal = erd.optDefaultValue
        _value = defaultVal.getNullablePrimitive
        _isDefaulted = true
        _value
      } else {
        DataValue.NoValue
      }
    mv
  }

  def dataValueAsString: JString = {
    if (_stringRep ne null) _stringRep
    else {
      dataValue.getAnyRef match {
        case s: String => s
        case arr: Array[Byte] => Misc.bytes2Hex(arr)
        case d: java.lang.Double => {
          //
          // Print these as needed in XML/XSD
          //
          if (d == Double.PositiveInfinity) XMLUtils.PositiveInfinityString
          else if (d == Double.NegativeInfinity) XMLUtils.NegativeInfinityString
          else d.toString
        }
        case f: java.lang.Float => {
          if (f == Float.PositiveInfinity) XMLUtils.PositiveInfinityString
          else if (f == Float.NegativeInfinity) XMLUtils.NegativeInfinityString
          else f.toString
        }
        case x => x.toString
      }
    }
  }

  def dataValueAsBigDecimal: JBigDecimal = {
    if (_bdRep ne null) _bdRep
    else {
      dataValue.getAnyRef match {
        case n: JBigDecimal => n
        case n: JNumber => Numbers.asJBigDecimal(n)
        case dc: DFDLCalendar => dc.toJBigDecimal
        case _ => Assert.usageError("value should not be converted to bigDecimal: " + dataValue)
      }
    }
  }

  override def isDefaulted: Boolean = {
    if (!_isNilled)
      dataValue // access this for side-effect that checks for default value.
    _isDefaulted
  }

  final def isEmpty: Boolean = {
    if (isNilled) false
    else {
      val nodeKind = erd.optPrimType.getOrElse(
        Assert.invariantFailed("optPrimType not defined for simple element")
      )
      if (nodeKind =:= NodeInfo.String) {
        dataValueAsString.length =#= 0
      } else if (nodeKind =:= NodeInfo.HexBinary) {
        dataValue.getByteArray.length =#= 0
      } else false
    }
  }

  /**
   * requireFinal is only ever used on unparse, and we never need to require a
   * simple type to be final during unparse. However, we do need to have an
   * implementation of this function, as DINode requires isFinal for parsing
   * and allowing the cleanup of unneeded DINodes.
   */
  override def requireFinal(): Unit = {
    Assert.invariantFailed("Should not requireFinal a simple type")
  }

  final def freeChildIfNoLongerNeeded(index: Int, doFree: Boolean): Unit = {
    Assert.invariantFailed("Should not try to remove a child of a simple type")
  }

  /**
   *
   * @return the value of this simple element as a Scala AnyRef, which is
   *         equivalent to a Java java.lang.Object.
   */
  override def getAnyRef: AnyRef = this.primType match {
    case NodeInfo.PrimType.Float => getFloat
    case NodeInfo.PrimType.Double => getDouble
    case NodeInfo.PrimType.Decimal => getDecimal
    case NodeInfo.PrimType.Integer => getInteger
    case NodeInfo.PrimType.Long => getLong
    case NodeInfo.PrimType.Int => getInt
    case NodeInfo.PrimType.Short => getShort
    case NodeInfo.PrimType.Byte => getByte
    case NodeInfo.PrimType.NonNegativeInteger => getNonNegativeInteger
    case NodeInfo.PrimType.UnsignedLong => getUnsignedLong
    case NodeInfo.PrimType.UnsignedInt => getUnsignedInt
    case NodeInfo.PrimType.UnsignedShort => getUnsignedShort
    case NodeInfo.PrimType.UnsignedByte => getUnsignedByte
    case NodeInfo.PrimType.String => getString
    case NodeInfo.PrimType.Boolean => getBoolean
    case NodeInfo.PrimType.HexBinary => getHexBinary
    case NodeInfo.PrimType.AnyURI => getURI
    case NodeInfo.PrimType.Date => getDate
    case NodeInfo.PrimType.Time => getTime
    case NodeInfo.PrimType.DateTime => getDateTime
  }

  override def getText: String = dataValueAsString
  override def getDecimal: JBigDecimal = withTry(dataValue.getBigDecimal)
  override def getDate: Calendar = withTry(dataValue.getDate.calendar)
  override def getTime: Calendar = withTry(dataValue.getTime.calendar)
  override def getDateTime: Calendar = withTry(dataValue.getDateTime.calendar)
  override def getHexBinary: Array[Byte] = withTry(dataValue.getByteArray)
  override def getBoolean: JBoolean = withTry(dataValue.getBoolean)
  override def getLong: JLong = withTry(Converter.asLong(dataValue.getAnyRef))
  override def getInt: JInt = withTry(Converter.asInt(dataValue.getAnyRef))
  override def getShort: JShort = withTry(Converter.asShort(dataValue.getAnyRef))
  override def getByte: JByte = withTry(Converter.asByte(dataValue.getAnyRef))
  override def getUnsignedInt: JLong = withTry(Converter.asLong(dataValue.getAnyRef))
  override def getUnsignedShort: JInt = withTry(Converter.asInt(dataValue.getAnyRef))
  override def getUnsignedByte: JShort = withTry(Converter.asShort(dataValue.getAnyRef))
  override def getDouble: JDouble = withTry(dataValue.getDouble)
  override def getFloat: JFloat = withTry(dataValue.getFloat)
  override def getInteger: JBigInt = withTry(dataValue.getBigInt)
  override def getNonNegativeInteger: JBigInt = withTry(dataValue.getBigInt)
  override def getString: JString = withTry(dataValue.getString)
  override def getURI: URI = withTry(dataValue.getURI)
  override def getUnsignedLong: JBigInt = withTry(Converter.asBigInt(dataValue.getAnyRef))

  private def withTry[A, B](f: => B): B = try {
    f
  } catch {
    case ite: InfosetTypeException => throw ite
    //
    // Catch other exceptions like numbers out of range,
    // and convert to InfosetTypeException
    //
    case e: Exception =>
      throw new InfosetTypeException(e)
  }

  private object Converter extends Numbers {
    override protected def errorThrower(message: JString): Nothing =
      throw new InfosetTypeException(message)
  }
}

/**
 * Complex elements have an array of DINodes. Nodes may either be DISimple for
 * simple types, DIComplex for complex types, or DIArray for arrays or optional
 * elements. Note that even if an element is nilled, there is still a DIElement
 * (i.e. a null child element does not mean the child is nilled). Also note
 * that in some cases Daffodil will remove children if they are determined they
 * are no longer needed to free up memory. It does this by setting the child to
 * null. So child slots can have a null value, but only in cases where Daffodil
 * should never need access to that child.
 */
sealed class DIComplex(override val erd: ElementRuntimeData)
  extends DIElement
  with DIComplexSharedImplMixin
  with InfosetComplexElement { diComplex =>

  override def metadata: ComplexElementMetadata = erd

  final override def isSimple = false
  final override def isComplex = true
  final override def isArray = false

  final override def isDefaulted: Boolean = {
    val res =
      if (erd.isNillable && isNilled)
        false
      else
        childNodes.forall {
          _.isDefaulted
        }
    res
  }

  private lazy val nfe = new InfosetComplexElementNotFinalException(this)

  override def valueStringForDebug: String = ""

  final def isEmpty: Boolean = false

  override def requireFinal(): Unit = {
    if (!isFinal) throw nfe
  }

  private val childNodes = new ArrayBuffer[DINode]

  private lazy val nameToChildNodeLookup =
    new java.util.HashMap[NamedQName, ArrayBuffer[DINode]]

  override def numChildren: Int = childNodes.length

  override def child(index: Int): DINode = childNodes(index)

  override def indexOf(item: DINode): Int = childNodes.indexOf(item)

  override def find(func: DINode => Boolean): Option[DINode] = childNodes.find(func)

  override def maybeLastChild: Maybe[DINode] = {
    val len = childNodes.length
    if (len > 0) {
      // note that if childNodes(len - 1) has been removed (i.e. set to null)
      // then this will by Maybe(null) which is a Nope. This is expected
      // behavior
      Maybe(childNodes(len - 1))
    } else Nope
  }

  final def getChild(erd: ElementRuntimeData, tunable: DaffodilTunables): DIElement = {
    getChild(erd.dpathElementCompileInfo.namedQName, tunable)
  }

  private def noQuerySupportCheck(nodes: Seq[DINode], nqn: NamedQName): Unit = {
    if (nodes.length > 1) {
      // might be more than one result
      // but we have to rule out there being an empty DIArray
      val withoutEmptyArrays = nodes.filter { node =>
        node match {
          case a: DIArray if a.length == 0 => false
          case _ => true
        }
      }
      if (withoutEmptyArrays.length > 1)
        erd.toss(InfosetAmbiguousNodeException(this, nqn))
    }
  }

  final def getChild(nqn: NamedQName, tunable: DaffodilTunables): DIElement = {
    val maybeNode = findChild(nqn, tunable)
    if (maybeNode.isDefined)
      maybeNode.get.asInstanceOf[DIElement]
    else
      erd.toss(new InfosetNoSuchChildElementException(this, nqn))
  }

  final def getChildArray(
    childERD: ElementRuntimeData,
    tunable: DaffodilTunables
  ): DIArray = {
    Assert.usage(childERD.isArray)

    getChildArray(childERD.dpathElementCompileInfo.namedQName, tunable)
  }

  final def getChildArray(nqn: NamedQName, tunable: DaffodilTunables): DIArray = {
    val maybeNode = findChild(nqn, tunable)
    if (maybeNode.isDefined) {
      maybeNode.get.asInstanceOf[DIArray]
    } else {
      erd.toss(new InfosetNoSuchChildElementException(this, nqn))
    }
  }

  def freeChildIfNoLongerNeeded(index: Int, doFree: Boolean): Unit = {
    val node = child(index)
    if (!node.erd.dpathElementCompileInfo.isReferencedByExpressions) {
      if (doFree) {
        // set to null so that the garbage collector can free this node
        childNodes(index) = null
      } else {
        node.wouldHaveBeenFreed = true
      }
    }
  }

  /*
   * When parsing unordered sequences it is possible to have non-contiguous arrays.
   * When parsed, these arrays show up as separate DIArrays in the infoset. We need
   * to combine these separate arrays and sort the child nodes into schema definition
   * order.
   * */
  final def flattenAndValidateChildNodes(pstate: ParseOrUnparseState, start: Int): Unit = {
    val (ordered, unordered) = childNodes.splitAt(start)
    childNodes.clear()
    childNodes ++= ordered
    val groups = unordered.groupBy(_.erd)
    unordered.clear()
    groups.foreach {
      case (erd, nodes) => {
        // Check min/maxOccurs validity while iterating over childNodes
        val min = erd.minOccurs
        val max = erd.maxOccurs
        val isUnbounded = max == -1

        // Flatten multiple DIArrays into the first one
        if (erd.isArray) {
          val a = nodes(0).asInstanceOf[DIArray]
          nodes.tail.foreach(b => a.concat(b.asInstanceOf[DIArray]))
          nodes.dropRightInPlace(nodes.length - 1)

          // Need to also remove duplicates from fastLookup
          val fastSeq = nameToChildNodeLookup.get(a.namedQName)
          if (fastSeq != null)
            fastSeq.dropRightInPlace(fastSeq.length - 1)

          // Validate min/maxOccurs for array
          val occurrence = nodes(0).numChildren
          if (isUnbounded && occurrence < min)
            pstate.validationError(
              "Element %s failed check of minOccurs='%s' and maxOccurs='unbounded', actual number of occurrences: %s",
              erd.namedQName,
              min,
              occurrence
            )
          else if (!isUnbounded && (occurrence < min || occurrence > max))
            pstate.validationError(
              "Element %s failed check of minOccurs='%s' and maxOccurs='%s', actual number of occurrences: %s",
              erd.namedQName,
              min,
              max,
              occurrence
            )

        } else {
          if (nodes.length > 1) {
            val diag = new InfosetMultipleScalarError(erd)
            pstate.setFailed(diag)
          }
        }

        unordered ++= nodes
      }
    }
    childNodes ++= unordered.sortBy(_.erd.position)
  }

  /**
   * Determines slotInParent from the ERD of the infoset element arg.
   * Hooks up the parent pointer of the new child to reference this.
   *
   * When slot contains an array, this appends to the end of the array.
   */
  def addChild(e: DIElement, tunable: DaffodilTunables): Unit = {
    if (e.runtimeData.isArray) {
      val childERD = e.runtimeData
      val needsNewArray =
        if (childNodes.isEmpty) {
          // This complex element has no children, so we must need to create a
          // new DIArray to add this array element
          true
        } else {
          val lastChild = childNodes(childNodes.length - 1)
          if (lastChild == null) {
            // The last child has been freed by Daffodil. That can only happen
            // if the array was finished so this new child cannot be part of
            // that array. We must create a new array
            true
          } else if (lastChild.erd ne childERD) {
            // The last child exists, but it is different than the new child.
            // We must create a new array.
            true
          } else {
            // The last child exists, and it has the same erd as this one. We
            // can just append this new child to that last child array. No need
            // to create a new one
            false
          }
        }

      if (needsNewArray) {
        // create a new array, then we will add the new child to that
        val ia = new DIArray(childERD, this, tunable.initialElementOccurrencesHint.toInt)
        addChildToFastLookup(ia)
        childNodes += ia
        _numChildren = childNodes.length
      }
      // Array is now always last, add the new child to it
      childNodes.last.asInstanceOf[DIArray].append(e)
    } else {
      addChildToFastLookup(e.asInstanceOf[DINode])
      childNodes += e.asInstanceOf[DINode]
      _numChildren = childNodes.length
    }
    e.setParent(this)
  }

  def addChildToFastLookup(node: DINode): Unit = {
    if (node.erd.dpathElementCompileInfo.isReferencedByExpressions) {
      val name = node.namedQName
      val fastSeq = nameToChildNodeLookup.get(name)
      if (fastSeq != null) {
        fastSeq += node
      } else {
        val ab = new ArrayBuffer[DINode]()
        ab += node
        nameToChildNodeLookup.put(name, ab)
      }
    }
  }

  def findChild(qname: NamedQName, tunable: DaffodilTunables): Maybe[DINode] = {
    findChild(qname, tunable.allowExternalPathExpressions)
  }

  /**
   * Find a child, using the preferred hash lookup, with an optional
   * linear search through the children.
   * @param qname
   * @param enableLinearSearchIfNotFound
   * @return
   */
  def findChild(qname: NamedQName, enableLinearSearchIfNotFound: Boolean): Maybe[DINode] = {
    val fastSeq = nameToChildNodeLookup.get(qname)
    if (fastSeq != null) {
      // Daffodil does not support query expressions yet, so there should only
      // be one item in the list
      noQuerySupportCheck(fastSeq.toSeq, qname)
      One(fastSeq(0))
    } else if (enableLinearSearchIfNotFound) {
      // Only DINodes used in expressions defined in the schema are added to
      // the nameToChildNodeLookup hashmap. If an expression defined outside of
      // the schema (like via the debugger) attempts to access an element that
      // was never used in a schema expression, it will never be found. If the
      // appropriate tunable is set, we will do a linear search to find the
      // element. Due to the slowness of this, this should only be enabled
      // during debugging or testing. Also note that we need to do a null check
      // since it's possible some children have been freed. Generally if this
      // tunable is set one should also have debugging enabled or disable
      // freeing of infoset nodes (see releaseUnneededInfoset). But those aren't
      // strictly required, so this avoids an NPE.
      val found = childNodes.filter { child =>
        child != null && child.erd.namedQName == qname
      }

      // Daffodil does not support query expressions yet, so there should be at
      // most one item found
      noQuerySupportCheck(found.toSeq, qname)
      Maybe.toMaybe(found.headOption)
    } else {
      Nope
    }
  }

  final def captureInto(cs: DIComplexSharedImplMixin): Unit = {
    Assert.invariant(_arraySize == MaybeInt.Nope)
    Assert.invariant(cs._arraySize == MaybeInt.Nope)

    cs._arraySize = if (_numChildren > 0) {
      childNodes.last match {
        case arr: DIArray => MaybeInt(arr.length.toInt)
        case _ => MaybeInt.Nope
      }
    } else { MaybeInt.Nope }
  }

  final def restoreFrom(cs: DIComplexSharedImplMixin): Unit = {
    var i = childNodes.length - 1

    // for each child we will remove, remove it from the fastLookup map.
    // this should always be the last element in the hashmap seq
    while (i >= cs._numChildren) {
      val childToRemove = childNodes(i)
      if (childToRemove.erd.dpathElementCompileInfo.isReferencedByExpressions) {
        val fastSeq = nameToChildNodeLookup.get(childToRemove.namedQName)
        Assert.invariant(fastSeq != null)
        if (fastSeq.length == 1) {
          // last one, remove the whole key entry
          nameToChildNodeLookup.remove(childToRemove.namedQName)
        } else {
          // not the last one, just drop the end
          fastSeq.dropRightInPlace(1)
        }
      }
      i -= 1
    }
    // now just quickly remove all those children
    childNodes.dropRightInPlace(childNodes.length - cs._numChildren)
    _numChildren = cs._numChildren

    if (cs._arraySize.isDefined && _numChildren > 0) {
      childNodes.last.asInstanceOf[DIArray].reduceToSize(cs._arraySize.get)
    }
  }

}

/*
 * Making this extend DIComplex eliminates a bunch of boundary
 * conditions having to do with the document root element.
 */
final class DIDocument(erd: ElementRuntimeData) extends DIComplex(erd) with InfosetDocument {

  /**
   * Set if this DIDocument is being attached to a DIElement just to establish the
   * invariant that there is a parent while we evaluate the expression to see if it has
   * a constant value
   */
  var isCompileExprFalseRoot: Boolean = false
}

object Infoset {

  def newElement(erd: ElementRuntimeData): DIElement = {
    if (erd.isSimpleType) new DISimple(erd)
    else new DIComplex(erd)
  }

  def newDocument(erd: ElementRuntimeData): InfosetDocument = {
    new DIDocument(erd)
  }

  /**
   * Create a new detached infoset with a single specified element. This
   * essentially creates another infoset with a completely differed DIDocument
   * so care should be taken to ensure things like expressions are not evaluated
   * or new elements are not added within the scope of this infoset, as it will
   * have no access to the true infoset. It is up to the caller to ensure the
   * return infoset element is used safely
   */
  def newDetachedElement(
    state: ParseOrUnparseState,
    erd: ElementRuntimeData
  ): DIElement = {
    val detachedDoc = Infoset.newDocument(erd).asInstanceOf[DIDocument]
    val detachedElem = Infoset.newElement(erd)
    detachedDoc.addChild(detachedElem, state.tunable)
    detachedElem
  }
}
