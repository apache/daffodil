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

package org.apache.daffodil.infoset

import java.lang.{ Boolean => JBoolean }
import java.lang.{ Number => JNumber }
import java.math.{ BigDecimal => JBigDecimal }
import java.util.HashMap
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicLong

import scala.collection.IndexedSeq
import scala.collection.mutable.ArrayBuffer

import org.apache.daffodil.api.DaffodilTunables
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.api.ThinDiagnostic
import org.apache.daffodil.calendar.DFDLCalendar
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.dsom.DPathElementCompileInfo
import org.apache.daffodil.equality.TypeEqual
import org.apache.daffodil.equality.ViewEqual
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.exceptions.ThinException
import org.apache.daffodil.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.infoset.DataValue.DataValuePrimitiveNullable
import org.apache.daffodil.io.DataOutputStream
import org.apache.daffodil.io.DirectOrBufferedDataOutputStream
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.EvalCache
import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.processors.ProcessingError
import org.apache.daffodil.processors.SimpleTypeRuntimeData
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.processors.parsers.PState
import org.apache.daffodil.util.LogLevel
import org.apache.daffodil.util.Logging
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe.Nope
import org.apache.daffodil.util.Maybe.One
import org.apache.daffodil.util.MaybeBoolean
import org.apache.daffodil.util.MaybeInt
import org.apache.daffodil.util.MaybeULong
import org.apache.daffodil.util.Misc
import org.apache.daffodil.util.Numbers
import org.apache.daffodil.xml.NS
import org.apache.daffodil.xml.NamedQName
import org.apache.daffodil.xml.XMLUtils

import passera.unsigned.ULong
import org.apache.daffodil.dsom.DPathCompileInfo

sealed trait DINode {

  def asSimple: DISimple = {
    this match {
      case diSimple: DISimple => diSimple
      case _ => {
        erd.toss(new InfosetWrongNodeType("simpleType", this)) // see comment with exception class definition for why this can happen
      }
    }
  }
  def isSimple: Boolean

  def asComplex: DIComplex = {
    this match {
      case diComplex: DIComplex => diComplex
      case _ => {
        erd.toss(new InfosetWrongNodeType("complexType", this)) // see comment with exception class definition for why this can happen
      }
    }
  }
  def isComplex: Boolean

  def asElement: DIElement =
    if (isSimple) asSimple
    else asComplex

  def isDefaulted: Boolean

  def children: Stream[DINode]
  def totalElementCount: Long
  def namedQName: NamedQName
  def erd: ElementRuntimeData

  /**
   * Can treat any DINode, even simple ones, as a container of other nodes.
   * This simplifies walking an infoset.
   */
  def contents: IndexedSeq[DINode]
  final def numChildren = contents.length

  def visit(handler: InfosetOutputter, removeHidden: Boolean = true): Unit

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
  extends ThinDiagnostic(One(node.erd.schemaFileLocation), Nope, Nope,
    One("Path step '%s' ambiguous. More than one infoset node corresponds to this name.\n" +
      "Query-style expressions are not supported."), nqn.toExtendedSyntax)
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
  extends ProcessingError("Expression Evaluation", Nope, Nope, "ComplexType element is not finalized.", node)
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
  extends ProcessingError("Expression Evaluation", Nope, Nope, "Expression expected %s to be a %s node.", node.namedQName, expectedType)
  with InfosetException

/**
 * Exception thrown if infoset doesn't have a child corresponding to the
 * slot being probed. E.g., expression evaluation reaching to a forward
 * sibling that has not yet been parsed.
 */
case class InfosetNoSuchChildElementException(val diComplex: DIComplex, nqn: NamedQName)
  extends ProcessingError("Expression Evaluation", Nope, Nope, "Child element %s does not exist.", nqn)
  with InfosetException with RetryableException

case class InfosetNoNextSiblingException(val diSimple: DISimple, val info: DPathElementCompileInfo)
  extends ProcessingError("Expression Evaluation", Nope, Nope, "Element %s does not have a nextSibling", info.namedQName)
  with InfosetException with RetryableException

case class InfosetNoInfosetException(val rd: Maybe[DPathCompileInfo])
  extends ProcessingError("Expression Evaluation", Nope, Nope, "There is no infoset%s", (if (rd.isEmpty) "." else " for path %s.".format(rd.get.path)))
  with InfosetException with RetryableException

/**
 * Indicates for simple types that there is no value, and has not been setNilled.
 *
 * For complex types indicates has not been setNilled.
 */
case class InfosetNoDataException(val diElement: DIElement, val erd: ElementRuntimeData)
  extends ProcessingError("Expression Evaluation", One(erd.schemaFileLocation), Nope, "Element %s does not have a value.", erd.namedQName)
  with InfosetException with RetryableException

case class InfosetArrayIndexOutOfBoundsException(val diArray: DIArray, val index: Long, val length: Long)
  extends ProcessingError("Expression Evaluation", Nope, Nope, "Value %d is out of range for the '%s' array with length %d", index, diArray.erd.namedQName, length)
  with InfosetException with RetryableException

/**
 * Don't catch this one. It's not restartable.
 */
case class InfosetFatalArrayIndexOutOfBoundsException(val diArray: DIArray, val index: Long, val length: Long)
  extends ProcessingError("Expression Evaluation", Nope, Nope, "Value %d is out of range for the '%s' array with length %d", index, diArray.erd.namedQName, length)
  with InfosetException

case class InfosetNoRootException(val diElement: DIElement, val erd: ElementRuntimeData)
  extends ProcessingError("Expression Evaluation", Nope, Nope, "No root element reachable from element %s.", erd.namedQName)
  with InfosetException with RetryableException

case class InfosetNoParentException(val diElement: DIElement, val erd: ElementRuntimeData)
  extends ProcessingError("Expression Evaluation", Nope, Nope, "No parent element for element %s.", erd.namedQName)
  with InfosetException

sealed abstract class InfosetLengthUnknownException(lengthState: LengthState, kind: String, val diElement: DIElement, val erd: ElementRuntimeData)
  extends ProcessingError("Expression Evaluation", Nope, Nope, "%s length unknown for element '%s'.", kind, erd.namedQName)
  with InfosetException with RetryableException

case class InfosetContentLengthUnknownException(lengthState: LengthState, override val diElement: DIElement, override val erd: ElementRuntimeData)
  extends InfosetLengthUnknownException(lengthState, "Content", diElement, erd)

case class InfosetValueLengthUnknownException(lengthState: LengthState, override val diElement: DIElement, override val erd: ElementRuntimeData)
  extends InfosetLengthUnknownException(lengthState, "Value", diElement, erd)

case class InfosetMultipleScalarError(val erd: ElementRuntimeData)
  extends ProcessingError("Scalar Element", Nope, Nope, "Multiple instances detected for scalar element %s", erd.namedQName)

/**
 * Used to determine if expressions can be evaluated without any nodes.
 * They all save/restore the current node, so this is the placeholder
 * they use for that purpose.
 */
final class FakeDINode extends DISimple(null) {
  private def die = throw new InfosetNoInfosetException(Nope)

  override def parent = die
  override def diParent = die
  override def setParent(p: InfosetComplexElement): Unit = die

  override def isNilled: Boolean = die
  override def setNilled(): Unit = die

  override def valid = die
  override def setValid(validity: Boolean): Unit = die

  override def dataValue: DataValuePrimitiveNullable = _value
  override def setDataValue(s: DataValuePrimitiveNullable): Unit = { _value = s }

  override def dataValueAsString: String = _value.toString
  override def isDefaulted: Boolean = die

  override def children = die

  override def contentLength: ContentLengthState = die
  override def valueLength: ValueLengthState = die
}

/**
 * Base for all terms, as there are these pseudo-nodes like DISequence which can have
 * runtime-valued properties, but which aren't infoset items.
 */
sealed trait DITerm {

  final lazy val parserEvalCache = new EvalCache
  final lazy val unparserEvalCache = new EvalCache

  def evalCache(state: ParseOrUnparseState) = state match {
    case p: PState => parserEvalCache
    case _ => unparserEvalCache
  }

  def trd: TermRuntimeData
  final def termRuntimeData = trd
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
sealed abstract class LengthState(ie: DIElement)
  extends Logging {

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
        System.err.println(th)
        res
      }
    }
  }

  var maybeStartDataOutputStream: Maybe[DataOutputStream] = Nope
  var maybeStartPos0bInBits: MaybeULong = MaybeULong.Nope
  var maybeEndDataOutputStream: Maybe[DataOutputStream] = Nope
  var maybeEndPos0bInBits: MaybeULong = MaybeULong.Nope
  var maybeComputedLength: MaybeULong = MaybeULong.Nope

  def copyFrom(other: LengthState): Unit = {
    this.maybeStartDataOutputStream = other.maybeStartDataOutputStream
    this.maybeStartPos0bInBits = other.maybeStartPos0bInBits
    this.maybeEndDataOutputStream = other.maybeEndDataOutputStream
    this.maybeEndPos0bInBits = other.maybeEndPos0bInBits
    this.maybeComputedLength = other.maybeComputedLength
  }

  def clear(): Unit = {
    maybeStartDataOutputStream = Nope
    maybeStartPos0bInBits = MaybeULong.Nope
    maybeEndDataOutputStream = Nope
    maybeEndPos0bInBits = MaybeULong.Nope
    maybeComputedLength = MaybeULong.Nope
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
      val dos = this.maybeStartDataOutputStream.get.asInstanceOf[DirectOrBufferedDataOutputStream]
      if (dos.maybeAbsBitPos0b.isDefined) {
        // we don't have absolute bit positions in this infoset element,
        // but the stream now has that information, so we have to migrate
        // that here.
        val thisRelStartPos0bInBits = this.maybeStartPos0bInBits.getULong
        val newStartBitPos0b = dos.toAbsolute(thisRelStartPos0bInBits)
        log(LogLevel.Debug, "%sgth for %s new absolute start pos: %s", flavor, ie.name, newStartBitPos0b)
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
        log(LogLevel.Debug, "%sgth for %s new absolute end pos: %s", flavor, ie.name, newEndBitPos0b)
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
    if (r) Assert.invariant(maybeStartDataOutputStream.isEmpty)
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
      if (maybeComputedLength.isDefined) {
        val len = maybeComputedLength.get
        log(LogLevel.Debug, "%sgth of %s is %s, (was already computed)", flavor, ie.name, len)
        return maybeComputedLength
      } else if (isStartUndef || isEndUndef) {
        log(LogLevel.Debug, "%sgth of %s cannot be computed yet. %s", flavor, ie.name, toString)
        MaybeULong.Nope
      } else if (isStartAbsolute && isEndAbsolute) {
        val len = maybeEndPos0bInBits.get - maybeStartPos0bInBits.get
        log(LogLevel.Debug, "%sgth of %s is %s, by absolute positions. %s", flavor, ie.name, len, toString)
        MaybeULong(len)
      } else if (isStartRelative && isEndRelative && (maybeStartDataOutputStream.get _eq_ maybeEndDataOutputStream.get)) {
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
        log(LogLevel.Debug, "%sgth of %s is %s, by relative positions in same data stream. %s", flavor, ie.name, len, toString)
        MaybeULong(len)
      } else if (isStartRelative && isEndRelative && maybeStartDataOutputStream.get.isFinished) {
        // if start and end DOSs are relative and different, but every DOS from
        // start inclusive to end excluste is finished, then we can calculate
        // the length based on relative positions. Note that it is fine for the
        // end DOS to be active because all we need are the relative bit
        // positive from the beginning of the DOS. We don't care about any bits
        // that maybe or may not occurr after it.

        // get partial lengths in the start and end DOS
        var len = (maybeStartDataOutputStream.get.relBitPos0b - maybeStartPos0bInBits.getULong) + maybeEndPos0bInBits.getULong

        // get all bits from DOS between start and end DOSs
        var dos = maybeEndDataOutputStream.get.asInstanceOf[DirectOrBufferedDataOutputStream].splitFrom
        val stop = maybeStartDataOutputStream.get.asInstanceOf[DirectOrBufferedDataOutputStream]
        while (dos.isFinished && (dos _ne_ stop)) {
          len = len + dos.relBitPos0b
          dos = dos.splitFrom
        }

        if (!dos.isFinished) {
          // found a non-finished DOS, can't calculate length
          log(LogLevel.Debug, "%sgth of %s is unknown due to unfinished output stream. %s", flavor, ie.name, toString)
          MaybeULong.Nope
        } else {
          log(LogLevel.Debug, "%sgth of %s is %s, by relative positions in same data stream. %s", flavor, ie.name, len, toString)
          MaybeULong(len.toLong)
        }
      } else {
        log(LogLevel.Debug, "%sgth of %s is unknown still. %s", flavor, ie.name, toString)
        MaybeULong.Nope
      }
    }
    maybeComputedLength = computed
    computed
  }

  def lengthInBits: ULong = {
    val mlib = maybeLengthInBits
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
    maybeStartPos0bInBits = MaybeULong(absPosInBits0b.longValue)
    maybeStartDataOutputStream = Nope
  }

  def setRelStartPos0bInBits(relPosInBits0b: ULong, dos: DataOutputStream): Unit = {
    maybeStartPos0bInBits = MaybeULong(relPosInBits0b.longValue)
    maybeStartDataOutputStream = One(dos)
  }

  def setAbsEndPos0bInBits(absPosInBits0b: ULong): Unit = {
    maybeEndPos0bInBits = MaybeULong(absPosInBits0b.longValue)
    maybeStartDataOutputStream = Nope
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

    e copyContentLengthFrom (this)
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
  with InfosetElement
  with DIElementSharedImplMixin {

  final override protected def allocContentLength = new ContentLengthState(this)
  final override protected def allocValueLength = new ValueLengthState(this)

  def isSimple: Boolean
  def isComplex: Boolean
  override final def name: String = erd.name
  override final def namespace: NS = erd.targetNamespace
  override final def namedQName = erd.namedQName
  override final def trd = erd

  /**
   * Tells us if the element was entirely created by defaulting elements.
   */
  def isDefaulted: Boolean

  /**
   * This is purely to make debugging easier.
   */
  override def toString = {
    val cl = Misc.getNameFromClass(this)
    val n = trd.name
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

  def isRoot = toParent match {
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

  override def setHidden: Unit = {
    _isHidden = true
  }

  final def runtimeData = erd
  protected final var _parent: InfosetComplexElement = null

  protected final var _isNilledSet: Boolean = false

  override def parent = _parent

  def diParent = _parent.asInstanceOf[DIComplex]

  override def setParent(p: InfosetComplexElement): Unit = {
    Assert.invariant(_parent eq null)
    _parent = p
  }

  private var _array: Maybe[InfosetArray] = Nope
  override def array = _array
  override def setArray(a: InfosetArray) = {
    _array = One(a)
  }

  /**
   * Used for just testing whether a node has the nil
   * indicators set. That is, dodges the expression evaluation
   * complexity where specific exceptions are thrown when you
   * ask about data that isn't known yet.
   */
  final def maybeIsNilled: MaybeBoolean = {
    if (!_isNilledSet) MaybeBoolean.Nope
    else MaybeBoolean(_isNilled)
  }

  /**
   * Tells if the element is nilled or not.
   *
   * Throws InfosetNoDataException if we don't yet
   * know if it is nil or not (i.e., hasn't be set, nor has
   * anything been set to indicate that it won't be nilled.)
   */
  def isNilled: Boolean

  override def setNilled(): Unit = {
    Assert.invariant(erd.isNillable)
    Assert.invariant(!_isNilled)
    _isNilled = true
    _isNilledSet = true
  }

  /**
   * valid = Nope means not checked
   * valid = One(true) means valid
   * valid = One(false) means invalid
   */
  override def valid = _validity
  override def setValid(validity: Boolean): Unit = { _validity = MaybeBoolean(validity) }
}

// This is not a mutable collection class on purpose.
// This forces use of while-loops and similar known-efficient
// code, rather than letting all sorts of map/flatmap compositions,
// which may or may not be optimized effectively.
//
final class DIArray(
  override val erd: ElementRuntimeData,
  val parent: DIComplex,
  initialSize: Int //TODO: really this needs to be adaptive, and resize upwards reasonably.
//A non-copying thing - list like, may be better, but we do need access to be
//constant time.
// FIXME: for streaming behavior, arrays are going to get elements removed from
// them when no longer needed. However, the array itself would still be growing
// without bound. So, replace this with a mutable map so that it can shrink
// as well as grow. // not saved. Needed only to get initial size.
)
  extends DINode with InfosetArray
  with DIFinalizable {

  private lazy val nfe = new InfosetArrayNotFinalException(this)

  override def requireFinal: Unit = {
    if (!isFinal) {
      // If this DIArray isn't final, either we haven't gotten all of its
      // children yet, or the array is empty and we'll never get its children.
      // In the former case,  we'll eventually get all the children and isFinal
      // will be set to true. However, in the latter case, isFinal will never
      // get set since the InfosetCursorFromXMLEventCursor, which sets the
      // isFinal state, doesn't know anything about the array. So we must check
      // if the parent isFinal. If the parent is final, that means we had a
      // zero-length array, and it should now be marked as final. If the parent
      // isn't final then we could still be wainting for events to come in, so
      // throw an nfe.
      if (parent.isFinal) {
        setFinal()
      } else {
        throw nfe
      }
    }
  }

  override def isSimple = false
  override def isComplex = false

  override def toString = "DIArray(" + namedQName + "," + _contents + ")"

  def namedQName = erd.namedQName

  protected final val _contents = new ArrayBuffer[DIElement](initialSize)

  override def children = _contents.toStream.asInstanceOf[Stream[DINode]]
  /**
   * Used to shorten array when backtracking out of having appended elements.
   */
  def reduceToSize(n: Int): Unit = {
    _contents.reduceToSize(n)
  }

  override def contents: IndexedSeq[DINode] = _contents

  /**
   * Note that occursIndex argument starts at position 1.
   */
  def getOccurrence(occursIndex1b: Long) = {
    if (occursIndex1b < 1)
      erd.toss(new InfosetFatalArrayIndexOutOfBoundsException(this, occursIndex1b, length)) // no blocking.
    if (occursIndex1b > length)
      erd.toss(new InfosetArrayIndexOutOfBoundsException(this, occursIndex1b, length)) // can be retried after blocking.
    _contents(occursIndex1b.toInt - 1)
  }

  @inline final def apply(occursIndex1b: Long) = getOccurrence(occursIndex1b)

  def append(ie: InfosetElement): Unit = {
    _contents += ie.asInstanceOf[DIElement]
    ie.setArray(this)
  }

  def concat(array: DIArray) = {
    val newContents = array.contents
    newContents.foreach(ie => {
      ie.asInstanceOf[InfosetElement].setArray(this)
      append(ie.asInstanceOf[InfosetElement])
    })
  }

  final def length: Long = _contents.length

  final def maybeMostRecentlyAddedChild(): Maybe[DIElement] = {
    val len = contents.length
    if (len == 0) Maybe.Nope
    else {
      val e = _contents(len - 1)
      Maybe(e)
    }
  }

  final def totalElementCount: Long = {
    var a: Long = 0
    _contents.foreach { c => a += c.totalElementCount }
    a
  }

  final def visit(handler: InfosetOutputter, removeHidden: Boolean = true): Unit = {
    // Do not create an event if there's nothing in the array or if the array
    // is hidden. Unfortunately, there is no way to tell if an array is hidden,
    // only its elements. But if the first element is hidden, they are all
    // hidden, so we check that to determine if the array is hidden.
    if (length > 0 && (!_contents(0).isHidden || !removeHidden)) {
      handler.startArray(this)
      _contents.foreach { _.visit(handler, removeHidden) }
      handler.endArray(this)
    }
  }

  final def isDefaulted: Boolean = children.forall { _.isDefaulted }
}

/**
 * Thrown when unparsing and demanding the dataValue of a DISimple
 * when there is no value yet, but the element has dfdl:outputValueCalc
 * expression.
 *
 * This should be caught in contexts that want to undertake on-demand
 * evaluation of the OVC expression.
 */
case class OutputValueCalcEvaluationException(cause: Exception)
  extends ThinException(cause)

sealed class DISimple(override val erd: ElementRuntimeData)
  extends DIElement
  with DISimpleSharedImplMixin
  with InfosetSimpleElement {

  final override def isSimple = true
  final override def isComplex = false

  def contents: IndexedSeq[DINode] = IndexedSeq.empty

  private var _stringRep: String = null
  private var _bdRep: JBigDecimal = null

  override def children: Stream[DINode] = Stream.Empty

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
  override def setDataValue(x: DataValuePrimitiveNullable): Unit = {
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
          case _: AtomicLong | _: AtomicInteger => Assert.invariantFailed("Unsupported type. %s of type %s.".format(x, Misc.getNameFromClass(x)))
          case n: java.lang.Number => n
          case u: java.net.URI => u
          case ar: AnyRef => Assert.invariantFailed("Unsupported type. %s of type %s.".format(x, Misc.getNameFromClass(x)))
        }
      }
    }
    _isNilled = false
    _isNilledSet = true
    _isDefaulted = false
    _validity = MaybeBoolean.Nope // we have not tested this new value.
    _unionMemberRuntimeData = Nope
  }

  override def isNilled: Boolean = {
    if (!erd.isNillable) false
    else if (_isNilledSet) {
      _isNilled
    } else {
      erd.toss(InfosetNoDataException(this, erd))
    }
  }

  def resetValue = {
    _isNilled = false
    _isNilledSet = false
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
  override def dataValue: DataValuePrimitiveNullable = {
    if (_value.isEmpty)
      if (erd.optDefaultValue.isDefined) {
        val defaultVal = erd.optDefaultValue
        if (defaultVal == DataValue.UseNilForDefault) {
          Assert.invariant(erd.isNillable)
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
    val mv = if (_value.isDefined)
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

  override def dataValueAsString = {
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

  final override def isEmpty: Boolean = {
    if (isNilled) false
    else {
      val nodeKind = erd.optPrimType.getOrElse(Assert.invariantFailed("optPrimType not defined for simple element"))
      if (nodeKind =:= NodeInfo.String) {
        dataValueAsString.length =#= 0
      } else if (nodeKind =:= NodeInfo.HexBinary) {
        dataValue.getByteArray.length =#= 0
      } else false
    }
  }

  override def totalElementCount = 1L

  final def visit(handler: InfosetOutputter, removeHidden: Boolean = true): Unit = {
    if (!this.isHidden || !removeHidden) {
      handler.startSimple(this)
      handler.endSimple(this)
    }
  }

}
/**
 * Arrays and complex elements have a notion of being finalized, when unparsing.
 * This is when we know that no more elements will be added to them, so things
 * like fn:count can return a value knowing it won't change, and fn:exists can
 * return false, knowing nobody will subsequently append the item that was being
 * questioned.
 */
sealed trait DIFinalizable {
  private var _isFinal: Boolean = false

  def setFinal(): Unit = { _isFinal = true }
  def isFinal = _isFinal

  /**
   * use to require it be finalized or throw the appropriate
   * Array or Complex exception.
   */
  def requireFinal: Unit
}

/**
 * Complex elements have an array of slots one per named child element.
 *
 * TODO: consider xs:choice - alternatives could share slots, but that would
 * add a lot of complexity, and the nil technique of storing null in a
 * slot to indicate a nilled element only works if we have a positive association
 * of slots to element-bases. If we were to share slots we'd need a different way
 * to indicate nil. A better approach for xs:choice would be a sparse table of
 * slots (whenever there are more than N - some threshold), so that we're not
 * allocating arrays of 200 slots just because there are 200 branches of a choice.
 *
 * A slot stores a Maybe[InfosetCommonMixin]. None means not present (yet, because it
 * hasn't been parsed yet, or it is an optional element (minOccurs 0, maxOccurs 1)
 * and is not present.) One[DISimple] or One[DIComplex] mean a required element
 * is present, or an optional element (minOccurs 0, maxOccurs 1) is present.
 *
 * A slot of a DIComplex should never be null.
 *
 * One[DIArray] means the slot is for a recurring element which can have 2+ instances.
 * The DIArray object's length gives the number of occurrences.
 */
sealed class DIComplex(override val erd: ElementRuntimeData)
  extends DIElement
  with DIComplexSharedImplMixin
  with InfosetComplexElement
  with DIFinalizable // with HasModelGroupMixin
  { diComplex =>

  final override def isSimple = false
  final override def isComplex = true

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

  override def requireFinal: Unit = {
    if (!isFinal) throw nfe
  }

  override def valueStringForDebug: String = ""

  final override def isEmpty: Boolean = false

  final override def isNilled: Boolean = {
    if (!erd.isNillable) false
    else if (_isNilledSet) {
      _isNilled
    } else if (this.isFinal) {
      // TODO: should we check that there are no children?
      false
    } else {
      erd.toss(new InfosetNoDataException(this, erd))
    }
  }

  val childNodes = new ArrayBuffer[DINode]
  //
  // TODO: Cleanup - Change below to use NonAllocatingMap to improve code style.
  lazy val nameToChildNodeLookup = new HashMap[NamedQName, ArrayBuffer[DINode]]

  override lazy val contents: IndexedSeq[DINode] = childNodes

  override def children = childNodes.toStream

  var hasVisibleChildren = false

  final def getChild(erd: ElementRuntimeData, tunable: DaffodilTunables): InfosetElement = {
    getChild(erd.dpathElementCompileInfo.namedQName, tunable)
  }

  private def noQuerySupportCheck(nodes: Seq[DINode], nqn: NamedQName) = {
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

  final def getChild(nqn: NamedQName, tunable: DaffodilTunables): InfosetElement = {
    val maybeNode = findChild(nqn, tunable)
    if (maybeNode.isDefined)
      maybeNode.get.asInstanceOf[InfosetElement]
    else
      erd.toss(new InfosetNoSuchChildElementException(this, nqn))
  }

  final def getChildArray(childERD: ElementRuntimeData, tunable: DaffodilTunables): InfosetArray = {
    Assert.usage(childERD.isArray)

    getChildArray(childERD.dpathElementCompileInfo.namedQName, tunable)
  }

  final def getChildArray(nqn: NamedQName, tunable: DaffodilTunables): InfosetArray = {
    val maybeNode = findChild(nqn, tunable)
    if (maybeNode.isDefined) {
      maybeNode.get.asInstanceOf[InfosetArray]
    } else {
      erd.toss(new InfosetNoSuchChildElementException(this, nqn))
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
    groups foreach {
      case (erd, nodes) => {
        // Check min/maxOccurs validity while iterating over childNodes
        val min = erd.minOccurs
        val max = erd.maxOccurs
        val isUnbounded = max == -1

        // Flatten multiple DIArrays into the first one
        if (erd.isArray) {
          val a = nodes(0).asInstanceOf[DIArray]
          nodes.tail.foreach(b => a.concat(b.asInstanceOf[DIArray]))
          nodes.reduceToSize(1)

          // Need to also remove duplicates from fastLookup
          val fastSeq = nameToChildNodeLookup.get(a.namedQName)
          if (fastSeq != null)
            fastSeq.reduceToSize(1)

          // Validate min/maxOccurs for array
          val occurrence = nodes(0).contents.length
          if (isUnbounded && occurrence < min)
            pstate.validationError("Element %s failed check of minOccurs='%s' and maxOccurs='unbounded', actual number of occurrences: %s", erd.namedQName, min, occurrence)
          else if (!isUnbounded && (occurrence < min || occurrence > max))
            pstate.validationError("Element %s failed check of minOccurs='%s' and maxOccurs='%s', actual number of occurrences: %s", erd.namedQName, min, max, occurrence)

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

  override def addChild(e: InfosetElement, tunable: DaffodilTunables): Unit = {
    if (!e.isHidden && !hasVisibleChildren) hasVisibleChildren = true
    if (e.runtimeData.isArray) {
      val childERD = e.runtimeData
      if (childNodes.isEmpty || childNodes.last.erd != childERD) {
        // no children, or last child is not a DIArray for
        // this element, create the DIArray
        val ia = new DIArray(childERD, this, tunable.initialElementOccurrencesHint.toInt)
        addChildToFastLookup(ia)
        childNodes += ia
        _numChildren = childNodes.length
        ia
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

  /**
   * Needed because at the point in the code where we need the
   * most recently added child, that node has already been popped from
   * the stack and the current node is its parent. This let's us get
   * a handle on the child just added without changing the invariants of
   * the way the node stack is handled in the PState/UState.
   */
  def maybeMostRecentlyAddedChild(): Maybe[DIElement] = {
    val len = contents.length
    if (len == 0) Maybe.Nope
    else {
      val lastChild = contents(len - 1)
      lastChild match {
        case a: DIArray => a.maybeMostRecentlyAddedChild()
        case e: DIElement => Maybe(e)
      }
    }
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
    val fastSeq = nameToChildNodeLookup.get(qname)
    if (fastSeq != null) {
      // Daffodil does not support query expressions yet, so there should only
      // be one item in the list
      noQuerySupportCheck(fastSeq, qname)
      One(fastSeq(0))
    } else if (tunable.allowExternalPathExpressions) {
      // Only DINodes used in expressions defined in the schema are added to
      // the nameToChildNodeLookup hashmap. If an expression defined outside of
      // the schema (like via the debugger) attempts to access an element that
      // was never used in a schema expression, it will never be found. If the
      // appropriate tunable is set, we will do a linear search to find the
      // element. Due to the slowness of this, this should only be enabled
      // during debugging or testing.
      val found = childNodes.filter(_.erd.namedQName == qname)

      // Daffodil does not support query expressions yet, so there should be at
      // most one item found
      noQuerySupportCheck(found, qname)
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
          fastSeq.reduceToSize(fastSeq.length - 1)
        }
      }
      i -= 1
    }
    // now just quickly remove all those children
    childNodes.reduceToSize(cs._numChildren)
    _numChildren = cs._numChildren

    if (cs._arraySize.isDefined && _numChildren > 0) {
      childNodes.last.asInstanceOf[DIArray].reduceToSize(cs._arraySize.get)
    }
  }

  override def totalElementCount: Long = {
    if (erd.isNillable && isNilled) return 1L
    var a: Long = 1
    childNodes.foreach(node => a += node.totalElementCount)
    a
  }

  override def visit(handler: InfosetOutputter, removeHidden: Boolean = true): Unit = {
    if (!this.isHidden || !removeHidden) {
      handler.startComplex(this)
      childNodes.foreach(node => node.visit(handler, removeHidden))
      handler.endComplex(this)
    }
  }
}

/*
 * Making this extend DIComplex eliminates a bunch of boundary
 * conditions having to do with the document root element.
 */
final class DIDocument(
  erd: ElementRuntimeData)
  extends DIComplex(erd)
  with InfosetDocument {
  var root: DIElement = null

  /**
   * Set if this DIDocument is being attached to a DIElement just to establish the
   * invariant that there is a parent while we evaluate the expression to see if it has
   * a constant value
   */
  var isCompileExprFalseRoot: Boolean = false

  def setRootElement(rootElement: InfosetElement, tunable: DaffodilTunables): Unit = {
    root = rootElement.asInstanceOf[DIElement]
    addChild(root, tunable)
  }

  override def addChild(child: InfosetElement, tunable: DaffodilTunables) =
    addChild(child)

  def addChild(child: InfosetElement): Unit = {
    /*
     * DIDocument normally only has a single child.
     * However, if said child wants to create a quasi-element (eg. if it is a simpleType with a typeCalc),
     * said element will need to be temporarily added as a second child to DIDocument.
     * When this happens, we do not set it as the root, but otherwise proceed normally
     */
    val node = child.asInstanceOf[DINode]
    childNodes += node
    if (node.erd.dpathElementCompileInfo.isReferencedByExpressions) {
      val ab = new ArrayBuffer[DINode]()
      ab += node
      nameToChildNodeLookup.put(node.namedQName, ab)
    }
    child.setParent(this)
    if (root == null)
      root = child.asInstanceOf[DIElement]
  }

  def getRootElement(): InfosetElement = {
    root
  }

  override def visit(handler: InfosetOutputter, removeHidden: Boolean = true): Unit = {
    assert(root != null)
    handler.startDocument()
    root.visit(handler, removeHidden)
    handler.endDocument()
  }
}

object Infoset {

  def newElement(erd: ElementRuntimeData): InfosetElement = {
    if (erd.isSimpleType) new DISimple(erd)
    else new DIComplex(erd)
  }

  def newDocument(erd: ElementRuntimeData): InfosetDocument = {
    new DIDocument(erd)
  }

  def newDocument(root: InfosetElement, tunable: DaffodilTunables): InfosetDocument = {
    val doc = newDocument(root.runtimeData)
    doc.setRootElement(root, tunable)
    doc
  }

  /**
   * Used to convert default value strings (from the XSD default property)
   * into the appropriate representation type.
   *
   * For use during compilation of a schema. Not for runtime, as this
   * can be slow.
   */
  // TODO: consolidate into the NodeInfo object where there is already similar
  // code. Or maybe consolidates into the DPath Conversions.scala code?
  //  def convertToInfosetRepType(primType: PrimType, value: String, context: ThrowsSDE): AnyRef = {
  //    import NodeInfo.PrimType._
  //    val res = primType match {
  //      case String => value
  //      case Int => value.toInt
  //      case Byte => value.toByte
  //      case Short => value.toShort
  //      case Long => value.toLong
  //      case Integer => new java.math.BigInteger(value)
  //      case Decimal => new java.math.BigDecimal(value)
  //      case UnsignedInt => {
  //        val res = java.lang.Long.parseLong(value)
  //        context.schemaDefinitionUnless(res >= 0, "Cannot convert %s to %s.", value, primType.name)
  //        res
  //      }
  //      case UnsignedByte => {
  //        val res = value.toShort
  //        context.schemaDefinitionUnless(res >= 0, "Cannot convert %s to %s.", value, primType.name)
  //        res
  //      }
  //      case UnsignedShort => {
  //        val res = value.toInt
  //        context.schemaDefinitionUnless(res >= 0, "Cannot convert %s to %s.", value, primType.name)
  //        res
  //      }
  //      case UnsignedLong => {
  //        val res = new java.math.BigInteger(value)
  //        context.schemaDefinitionUnless(res.doubleValue >= 0, "Cannot convert %s to %s.", value, primType.name)
  //        res
  //      }
  //      case NonNegativeInteger => {
  //        val res = new java.math.BigInteger(value)
  //        context.schemaDefinitionUnless(res.doubleValue >= 0, "Cannot convert %s to %s.", value, primType.name)
  //        res
  //      }
  //      case Double => {
  //        value match {
  //          case XMLUtils.PositiveInfinityString => scala.Double.PositiveInfinity
  //          case XMLUtils.NegativeInfinityString => scala.Double.NegativeInfinity
  //          case XMLUtils.NaNString => scala.Double.NaN
  //          case _ => value.toDouble
  //        }
  //      }
  //      case Float => {
  //        value match {
  //          case XMLUtils.PositiveInfinityString => scala.Float.PositiveInfinity
  //          case XMLUtils.NegativeInfinityString => scala.Float.NegativeInfinity
  //          case XMLUtils.NaNString => scala.Float.NaN
  //          case _ => value.toFloat
  //        }
  //      }
  //      case HexBinary => Misc.hex2Bytes(value) // convert hex constant into byte array
  //      case Boolean => {
  //        if (value == "true") true
  //        else if (value == "false") false
  //        else context.schemaDefinitionError("Cannot convert %s to %s.", value, primType.name)
  //      }
  //      case Time => {
  //        val cal = new GregorianCalendar()
  //        val pos = new java.text.ParsePosition(0)
  //        new com.ibm.icu.text.SimpleDateFormat("HH:mm:ssZZZZZ").parse(value, cal, pos)
  //        DFDLTime(cal, true)
  //      }
  //      case DateTime => {
  //        val cal = new GregorianCalendar()
  //        val pos = new java.text.ParsePosition(0)
  //        new com.ibm.icu.text.SimpleDateFormat("uuuu-MM-dd'T'HH:mm:ssZZZZZ").parse(value, cal, pos)
  //        DFDLDateTime(cal, true)
  //      }
  //      case Date => {
  //        val cal = new GregorianCalendar()
  //        val pos = new java.text.ParsePosition(0)
  //        new com.ibm.icu.text.SimpleDateFormat("uuuu-MM-dd").parse(value, cal, pos)
  //        DFDLDate(cal, true)
  //      }
  //    }
  //    res.asInstanceOf[AnyRef]
  //  }

}
