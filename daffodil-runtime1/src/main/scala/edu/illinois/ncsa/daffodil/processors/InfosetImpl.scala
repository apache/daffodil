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

import java.lang.{ Boolean => JBoolean }
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.util.MaybeInt
import edu.illinois.ncsa.daffodil.util.MaybeULong
import scala.collection.mutable.ArrayBuffer
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.xml.NS
import com.ibm.icu.util.GregorianCalendar
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import scala.xml.Elem
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import scala.xml.MetaData
import scala.annotation.tailrec
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.util.Misc
import scala.xml.Null
import edu.illinois.ncsa.daffodil.api.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.xml.NamedQName
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.dsom.DPathElementCompileInfo
import edu.illinois.ncsa.daffodil.dsom.DiagnosticImplMixin
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.exceptions.ThinThrowable
import edu.illinois.ncsa.daffodil.util.MaybeBoolean
import scala.collection.IndexedSeq
import edu.illinois.ncsa.daffodil.dpath.AsIntConverters._
import edu.illinois.ncsa.daffodil.util.MaybeULong
import passera.unsigned.ULong
import edu.illinois.ncsa.daffodil.io.DataOutputStream
import edu.illinois.ncsa.daffodil.io.DirectOrBufferedDataOutputStream
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.calendar.DFDLDateTime
import edu.illinois.ncsa.daffodil.calendar.DFDLTime
import edu.illinois.ncsa.daffodil.calendar.DFDLDate
import edu.illinois.ncsa.daffodil.calendar.DFDLCalendar
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicLong

sealed trait DINode {
  def toXML(removeHidden: Boolean = true, showFormatInfo: Boolean = false): scala.xml.NodeSeq

  def asSimple: DISimple = {
    this match {
      case diSimple: DISimple => diSimple
      case _ => {
        throw new InfosetWrongNodeType("simpleType", this) // see comment with exception class definition for why this can happen
      }
    }
  }

  def asComplex: DIComplex = {
    this match {
      case diComplex: DIComplex => diComplex
      case _ => {
        throw new InfosetWrongNodeType("complexType", this) // see comment with exception class definition for why this can happen
      }
    }
  }

  def children: Stream[DINode]
  def toWriter(writer: java.io.Writer, removeHidden: Boolean = true, indentStep: Int = 2, indentLevel: Int = 0): Unit
  def totalElementCount: Long
  def namedQName: NamedQName
  def erd: ElementRuntimeData

  /**
   * Can treat any DINode, even simple ones, as a container of other nodes.
   * This simplifies walking an infoset.
   */
  def filledSlots: IndexedSeq[DINode]
  final def numChildren = filledSlots.length

}

/**
 * Marker trait that identifies exceptions which indicate an action that can block
 * forward-referencing expressions that occur during unparsing. These actions
 * can be retried once the available information has "arrived" in the infoset or
 * the state of the unparser.
 */
trait RetryableException

trait InfosetException extends DiagnosticImplMixin with ThinThrowable

trait InfosetNodeNotFinalException extends InfosetException with RetryableException {
  def node: DINode
}

case class InfosetArrayNotFinalException(override val node: DIArray)
  extends ProcessingError("Error", Nope, Nope, "Array is not finalized.", node)
  with InfosetNodeNotFinalException

case class InfosetComplexElementNotFinalException(override val node: DIComplex)
  extends ProcessingError("Error", Nope, Nope, "ComplexType element is not finalized.", node)
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
  extends ProcessingError("Error", Nope, Nope, "Expression expected %s to be a %s node.", node.namedQName, expectedType)
  with InfosetException

/**
 * Exception thrown if infoset doesn't have a child corresponding to the
 * slot being probed. E.g., expression evaluation reaching to a forward
 * sibling that has not yet been parsed.
 */
case class InfosetNoSuchChildElementException(val diComplex: DIComplex, val info: DPathElementCompileInfo)
  extends ProcessingError("Error", Nope, Nope, "Child element %s does not exist.", info.namedQName)
  with InfosetException with RetryableException

case class InfosetNoInfosetException(val rd: Maybe[RuntimeData])
  extends ProcessingError("Error", Nope, Nope, "There is no infoset%s", (if (rd.isEmpty) "." else " for path %s.".format(rd.get.path)))
  with InfosetException with RetryableException

/**
 * Indicates for simple types that there is no value, and has not been setNilled.
 *
 * For complex types indicates has not been setNilled.
 */
case class InfosetNoDataException(val diElement: DIElement, val erd: ElementRuntimeData)
  extends ProcessingError("Error", One(erd.schemaFileLocation), Nope, "Element %s does not have a value.", erd.namedQName)
  with InfosetException with RetryableException

case class InfosetArrayIndexOutOfBoundsException(val diArray: DIArray, val index: Long, val length: Long)
  extends ProcessingError("Error", Nope, Nope, "Value %d is out of range for the '%s' array with length %d", index, diArray.erd.namedQName, length)
  with InfosetException with RetryableException

/**
 * Don't catch this one. It's not restartable.
 */
case class InfosetFatalArrayIndexOutOfBoundsException(val diArray: DIArray, val index: Long, val length: Long)
  extends ProcessingError("Error", Nope, Nope, "Value %d is out of range for the '%s' array with length %d", index, diArray.erd.namedQName, length)
  with InfosetException

case class InfosetNoRootException(val diElement: DIElement, val erd: ElementRuntimeData)
  extends ProcessingError("Error", Nope, Nope, "No root element reachable from element %s.", erd.namedQName)
  with InfosetException with RetryableException

case class InfosetNoParentException(val diElement: DIElement, val erd: ElementRuntimeData)
  extends ProcessingError("Error", Nope, Nope, "No parent element for element %s.", erd.namedQName)
  with InfosetException

sealed abstract class InfosetLengthUnknownException(lengthState: LengthState, kind: String, val diElement: DIElement, val erd: ElementRuntimeData)
  extends ProcessingError("Error", Nope, Nope, "%s length unknown for element %s.", kind, erd.namedQName)
  with InfosetException with RetryableException {

  lazy val blockingDOS = lengthState.diagnoseNoLength()

  override def componentText = "BlockingDOS(" + blockingDOS + ")"
}

case class InfosetContentLengthUnknownException(lengthState: LengthState, override val diElement: DIElement, override val erd: ElementRuntimeData)
  extends InfosetLengthUnknownException(lengthState, "Content", diElement, erd)

case class InfosetValueLengthUnknownException(lengthState: LengthState, override val diElement: DIElement, override val erd: ElementRuntimeData)
  extends InfosetLengthUnknownException(lengthState, "Value", diElement, erd)

/**
 * Used to determine if expressions can be evaluated without any nodes.
 * They all save/restore the current node, so this is the placeholder
 * they use for that purpose.
 */
final class FakeDINode extends DISimple(null) {
  private def die = throw new InfosetNoInfosetException(Nope)

  override def toXML(removeHidden: Boolean = true, showFormatInfo: Boolean = false): scala.xml.NodeSeq = die
  override def removeHiddenElements(): InfosetElement = die

  override def parent = die
  override def diParent = die
  override def setParent(p: InfosetComplexElement): Unit = die

  override def isNilled: Boolean = die
  override def setNilled(): Unit = die

  override def valid = die
  override def setValid(validity: Boolean): Unit = die

  override def dataValue: AnyRef = _value
  override def setDataValue(s: AnyRef): Unit = { _value = asAnyRef(s) }

  override def dataValueAsString: String = _value.toString
  override def isDefaulted: Boolean = die

  override def children = die

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

  protected final def dafPrefix = {
    val ee = trd.dpathCompileInfo.immediateEnclosingCompileInfo.getOrElse {
      trd.dpathCompileInfo
    }
    val pre = ee.namespaces.getPrefix(XMLUtils.dafintURI.uri.toString())
    Assert.invariant(pre ne null)
    pre
  }

  /**
   * String suitable for use in the text of a Processing Instruction.
   *
   * The text is a pseudo-XML string.
   */
  protected final def fmtInfo: Maybe[String] = {
    // val mgXML = modelGroupsPseudoXML
    val pecXML = parserEvalCache.toPseudoXML()
    val uecXML = unparserEvalCache.toPseudoXML()
    val puxml = {
      // mgXML +
      (if (pecXML =:= "") "" else "\n" + pecXML) +
        (if (uecXML =:= "") "" else "\n" + uecXML)
    }
    Maybe(if (puxml =:= "") null else puxml)
  }

  protected final def addFmtInfo(elem: scala.xml.Elem, showFormatInfo: Boolean): scala.xml.Elem = {
    if (!showFormatInfo) return elem
    val maybeFI = fmtInfo
    val res =
      if (maybeFI.isEmpty) elem
      else {
        val fi = maybeFI.value
        val pi = new scala.xml.ProcInstr("formatInfo", fi)
        val res = elem.copy(child = elem.child :+ pi)
        res
      }
    res
  }
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

  private var maybeStartDataOutputStream: Maybe[DataOutputStream] = Nope
  private var maybeStartPos0bInBits: MaybeULong = MaybeULong.Nope
  private var maybeEndDataOutputStream: Maybe[DataOutputStream] = Nope
  private var maybeEndPos0bInBits: MaybeULong = MaybeULong.Nope
  private var maybeComputedLength: MaybeULong = MaybeULong.Nope

  def copyFrom(other: LengthState) {
    this.maybeStartDataOutputStream = other.maybeStartDataOutputStream
    this.maybeStartPos0bInBits = other.maybeStartPos0bInBits
    this.maybeEndDataOutputStream = other.maybeEndDataOutputStream
    this.maybeEndPos0bInBits = other.maybeEndPos0bInBits
    this.maybeComputedLength = other.maybeComputedLength
  }

  def clear() {
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
  private def recheckStreams() {
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

  private def isStartUndef = {
    val r = maybeStartPos0bInBits.isEmpty
    if (r) Assert.invariant(maybeStartDataOutputStream.isEmpty)
    r
  }

  private def isEndUndef = {
    val r = maybeEndPos0bInBits.isEmpty
    if (r) Assert.invariant(maybeStartDataOutputStream.isEmpty)
    r
  }

  private def isStartAbsolute = {
    maybeStartPos0bInBits.isDefined &&
      maybeStartDataOutputStream.isEmpty
  }

  private def isEndAbsolute = {
    maybeEndPos0bInBits.isDefined &&
      maybeEndDataOutputStream.isEmpty
  }

  private def isStartRelative = {
    val res = maybeStartDataOutputStream.isDefined
    if (res) {
      Assert.invariant(maybeStartPos0bInBits.isDefined)
      // Assert.invariant(maybeStartDataOutputStream.get.maybeAbsBitPos0b.isEmpty)
    }
    res
  }

  private def isEndRelative = {
    val res = maybeEndDataOutputStream.isDefined
    if (res) {
      Assert.invariant(maybeEndPos0bInBits.isDefined)
      //  Assert.invariant(maybeEndDataOutputStream.get.maybeAbsBitPos0b.isEmpty)
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

  def setAbsStartPos0bInBits(absPosInBits0b: ULong) {
    maybeStartPos0bInBits = MaybeULong(absPosInBits0b.longValue)
    maybeStartDataOutputStream = Nope
  }

  def setRelStartPos0bInBits(relPosInBits0b: ULong, dos: DataOutputStream) {
    maybeStartPos0bInBits = MaybeULong(relPosInBits0b.longValue)
    maybeStartDataOutputStream = One(dos)
  }

  def setAbsEndPos0bInBits(absPosInBits0b: ULong) {
    maybeEndPos0bInBits = MaybeULong(absPosInBits0b.longValue)
    maybeStartDataOutputStream = Nope
  }

  def setRelEndPos0bInBits(relPosInBits0b: ULong, dos: DataOutputStream) {
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
    throw new InfosetContentLengthUnknownException(this, ie, ie.runtimeData)
  }

}

class ValueLengthState(ie: DIElement) extends LengthState(ie) {

  override def flavor = "valueLen"

  override def throwUnknown = {
    Assert.invariant(ie ne null)
    throw new InfosetValueLengthUnknownException(this, ie, ie.runtimeData)
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
  final protected def copyContentLengthFrom(e: DIElementSharedMembersMixin) {
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
  final protected def copyValueLengthFrom(e: DIElementSharedMembersMixin) {
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
  final def contentLength = {
    if (_contentLength eq null) {
      _contentLength = allocContentLength
    }
    _contentLength
  }

  final def valueLength = {
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

  override def restoreInto(e: DIElement) {
    e._isNilled = this._isNilled
    e._validity = this._validity

    e copyContentLengthFrom (this)
    e.copyValueLengthFrom(this)
  }

  override def captureFrom(e: DIElement) {
    this._isNilled = e._isNilled
    this._validity = e._validity

    this.copyContentLengthFrom(e)
    this.copyValueLengthFrom(e)
  }

  override def clear() {
    this._isNilled = false
    this._validity = MaybeBoolean.Nope
    contentLength.clear()
    valueLength.clear()
  }

}

sealed trait DIComplexSharedMembersMixin {

  final var _lastSlotAdded: Int = -1
  final var _arraySize: MaybeInt = MaybeInt.Nope
}

sealed trait DIComplexSharedImplMixin
  extends DIElementSharedInterface
  with DIComplexSharedMembersMixin {

  abstract override def restoreInto(e: DIElement) {
    val c = e.asInstanceOf[DIComplex]
    c.restoreFrom(this)
    c._lastSlotAdded = this._lastSlotAdded
    super.restoreInto(e)
  }

  abstract override def captureFrom(e: DIElement) {
    val c = e.asInstanceOf[DIComplex]
    c.captureInto(this)
    this._lastSlotAdded = c._lastSlotAdded
    super.captureFrom(e)
  }

  abstract override def clear() {
    _lastSlotAdded = -1
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

  protected var _value: AnyRef = null
}

sealed trait DISimpleSharedImplMixin
  extends DIElementSharedInterface
  with DISimpleSharedMembersMixin {

  abstract override def restoreInto(e: DIElement) {
    super.restoreInto(e)
    val s = e.asInstanceOf[DISimple]
    s._isDefaulted = this._isDefaulted
    s._value = this._value
  }

  abstract override def captureFrom(e: DIElement) {
    super.captureFrom(e)
    val s = e.asInstanceOf[DISimple]
    this._isDefaulted = s._isDefaulted
    this._value = s._value
  }

  abstract override def clear() {
    super.clear()
    _isDefaulted = false
    _value = null
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
   * This is purely to make debugging easier.
   */
  override def toString = {
    val cl = Misc.getNameFromClass(this)
    val n = trd.name
    cl + "(name='" + n + "' " + contentLength.toString() + " " + valueLength.toString() + ")"
  }

  def isRoot = parent match {
    case doc: DIDocument => !doc.isCompileExprFalseRoot
    case _ => false
  }

  def isRootDoc = this match {
    case doc: DIDocument => !doc.isCompileExprFalseRoot
    case _ => false
  }

  def toRootDoc: DIComplex = toRootDoc1(this)

  private def toRootDoc1(orig: DIElement): DIComplex = {
    if (isRootDoc) this.asInstanceOf[DIDocument]
    else if (isRoot) diParent
    else {
      parent match {
        case null =>
          throw new InfosetNoRootException(orig, erd)
        case elt: DIElement => elt.toRootDoc1(orig)
        case _ =>
          throw new InfosetNoRootException(orig, erd)
      }
    }
  }

  def toParent = {
    if (parent eq null)
      throw new InfosetNoParentException(this, erd)
    diParent
  }

  /**
   * Note: there is no infoset data member for isHidden. A hidden group is
   * a DFDL schema characteristic for a model group. Elements inside it will
   * have (their element base) isHidden statically on the schema. So there is
   * no notion of creating an infoset element then making it hidden by marking
   * it in some way. Rather, the corresponding elementRuntimeData tells you whether
   * it is hidden or not.
   *
   * When we convert to XML, then if we want to preserve information about
   * things being hidden (for inspection by looking at the XML) then we
   * need to add an attribute. But for the infoset itself, we don't need it.
   */
  final def isHidden: Boolean = erd.isHidden

  final def runtimeData = erd
  protected final var _parent: InfosetComplexElement = null

  protected final var _isNilledSet: Boolean = false

  override def parent = _parent
  def diParent = _parent.asInstanceOf[DIComplex]
  override def setParent(p: InfosetComplexElement) {
    Assert.invariant(_parent eq null)
    _parent = p
  }

  private var _array: Maybe[InfosetArray] = Nope
  override def array = _array
  override def setArray(a: InfosetArray) = {
    Assert.invariant(_array == Nope)
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
    MaybeBoolean(_isNilled)
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
  override def setValid(validity: Boolean) { _validity = MaybeBoolean(validity) }

  private def uniqueScopeString: String = {
    val nsbStart = erd.minimizedScope
    val nsbEnd = if (isRoot) scala.xml.TopScope else diParent.erd.minimizedScope
    if (nsbStart == nsbEnd) {
      "" // nothing to add, return empty string
    } else {
      val str = XMLUtils.uniqueNamespaceBindingsToString(nsbStart, nsbEnd)
      " " + str
    }
  }

  private def pre = erd.thisElementsNamespacePrefix
  private lazy val qn = if (pre == null | pre == "") erd.name else pre + ":" + erd.name
  protected final lazy val nilledTag = "<" + qn + uniqueScopeString + " xsi:nil=\"true\" />"
  protected final lazy val startTag = "<" + qn + uniqueScopeString + ">"
  protected final lazy val endTag = "</" + qn + ">"

  protected def writeContents(writer: java.io.Writer, removeHidden: Boolean, indentStep: Int, indentLevel: Int): Unit

  override def toWriter(writer: java.io.Writer, removeHidden: Boolean = true, indentStep: Int = 2, indentLevel: Int = 0) {
    if (isHidden && removeHidden) return
    val indentString = " " * (indentStep * indentLevel)
    writer.write(indentString)
    if (isNilled) {
      writer.write(nilledTag)
    } else {
      writer.write(startTag)
      if (erd.isComplexType) {
        writer.write("\n")
      }
      writeContents(writer, removeHidden, indentStep, indentLevel + 1)
      if (erd.isComplexType) {
        writer.write(indentString)
      }
      writer.write(endTag)
    }
    writer.write("\n")
  }

}

// This is not a mutable collection class on purpose.
// This forces use of while-loops and similar known-efficient
// code, rather than letting all sorts of map/flatmap compositions,
// which may or may not be optimized effectively.
//
final class DIArray(
  override val erd: ElementRuntimeData,
  val parent: DIComplex)
  extends DINode with InfosetArray
  with DIFinalizable {

  private lazy val nfe = new InfosetArrayNotFinalException(this)

  override def requireFinal {
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
        isFinal = true
      } else {
        throw nfe
      }
    }
  }

  override def toString = "DIArray(" + namedQName + "," + _contents + ")"

  def namedQName = erd.namedQName

  private val initialSize = DaffodilTunableParameters.initialElementOccurrencesHint.toInt
  //TODO: really this needs to be adaptive, and resize upwards reasonably.
  //A non-copying thing - list like, may be better, but we do need access to be
  //constant time.
  // FIXME: for streaming behavior, arrays are going to get elements removed from
  // them when no longer needed. However, the array itself would still be growing
  // without bound. So, replace this with a mutable map so that it can shrink
  // as well as grow.
  protected final val _contents = new ArrayBuffer[DIElement](initialSize)

  override def children = _contents.toStream.asInstanceOf[Stream[DINode]]
  /**
   * Used to shorten array when backtracking out of having appended elements.
   */
  def reduceToSize(n: Int) {
    _contents.reduceToSize(n)
  }

  override def filledSlots: IndexedSeq[DINode] = _contents

  /**
   * Note that occursIndex argument starts at position 1.
   */
  def getOccurrence(occursIndex1b: Long) = {
    if (occursIndex1b < 1)
      throw new InfosetFatalArrayIndexOutOfBoundsException(this, occursIndex1b, length) // no blocking.
    if (occursIndex1b > length)
      throw new InfosetArrayIndexOutOfBoundsException(this, occursIndex1b, length) // can be retried after blocking.
    _contents(occursIndex1b.toInt - 1)
  }

  @inline final def apply(occursIndex1b: Long) = getOccurrence(occursIndex1b)

  def append(ie: InfosetElement): Unit = {
    _contents += ie.asInstanceOf[DIElement]
    ie.setArray(this)
  }

  final def length: Long = _contents.length

  final def toXML(removeHidden: Boolean = true, showFormatInfo: Boolean = false): scala.xml.NodeSeq = {
    _contents.flatMap { _.toXML(removeHidden, showFormatInfo) }
  }

  final def toWriter(writer: java.io.Writer, removeHidden: Boolean = true, indentStep: Int = 2, indentLevel: Int = 0) {
    _contents.foreach { _.toWriter(writer, removeHidden, indentStep, indentLevel) }
  }

  final def totalElementCount: Long = {
    var a: Long = 0
    _contents.foreach { c => a += c.totalElementCount }
    a
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
case class OutputValueCalcEvaluationException(val cause: Exception)
  extends Exception(cause) with ThinThrowable with DiagnosticImplMixin

sealed class DISimple(override val erd: ElementRuntimeData)
  extends DIElement
  with DISimpleSharedImplMixin
  with InfosetSimpleElement {

  final override def isSimple = true
  final override def isComplex = false

  def filledSlots: IndexedSeq[DINode] = IndexedSeq.empty

  private var _stringRep: String = null

  override def children: Stream[DINode] = Stream.Empty

  def setDefaultedDataValue(defaultedValue: AnyRef) = {
    setDataValue(defaultedValue)
    _isDefaulted = true
  }

  /**
   * Parsing of a text number first does setDataValue to a string, then a conversion does overwrite data value
   * with a number. Unparsing does setDataValue to a value, then overwriteDataValue to a string.
   */
  override def setDataValue(x: AnyRef) {
    Assert.invariant(!hasValue)
    overwriteDataValue(x)
  }

  def overwriteDataValue(x: AnyRef) {
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
    x match {
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
          _stringRep = x.asInstanceOf[String]
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
        _value = x match {
          case dc: DFDLCalendar => dc
          case arb: Array[Byte] => arb
          case b: JBoolean => b
          case  _: AtomicLong | _: AtomicInteger => Assert.invariantFailed("Unsupported type. %s of type %s.".format(x, Misc.getNameFromClass(x)))
          case n: java.lang.Number => n
          case ar: AnyRef => Assert.invariantFailed("Unsupported type. %s of type %s.".format(x, Misc.getNameFromClass(x)))
        }
      }
    }
    _isNilled = false
    _isNilledSet = true
    _isDefaulted = false
    _validity = MaybeBoolean.Nope // we have not tested this new value.
  }

  override def isNilled: Boolean = {
    if (!erd.isNillable) false
    else if (_isNilledSet) {
      _isNilled
    } else {
      throw new InfosetNoDataException(this, erd)
    }
  }

  def resetValue = {
    _isNilled = false
    _isNilledSet = false
    _isDefaulted = false
    _validity = MaybeBoolean.Nope // we have not tested this new value.
    _stringRep = null
    _value = null
  }

  def hasValue: Boolean = !_isNilled && _value != null
  /**
   * Obtain the data value. Implements default
   * values, and outputValueCalc for unparsing.
   */
  override def dataValue: AnyRef = {
    if (_value == null)
      if (erd.optDefaultValue.isDefined) {
        val defaultVal = erd.optDefaultValue.get
        _value = defaultVal
        _isDefaulted = true
      } else {
        throw new InfosetNoDataException(this, erd)
      }
    this.erd.schemaDefinitionUnless(_value != null, "Value has not been set.")
    _value
  }

  override def dataValueAsString = {
    if (_stringRep ne null) _stringRep
    else {
      dataValue match {
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
        case _ => dataValue.toString
      }
    }
  }

  override def isDefaulted: Boolean = {
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
        dataValue.asInstanceOf[Array[Byte]].length =#= 0
      } else false
    }
  }

  override def removeHiddenElements(): InfosetElement = this

  /**
   * For future - if string is big enough or other criteria
   * Then maybe escape it by wrapping with <![CDATA[...]]>
   * but note that any internal appearance of ]]> must be
   * followed by inserting <![CDATA[. (These cannot nest)
   */
  private def shouldEscapeWithCData(s: String): Maybe[String] = {
    Nope
  }

  override final def writeContents(writer: java.io.Writer, removeHidden: Boolean, indentStep: Int, indentLevel: Int) {
    // this element might not have a value if we are writing the contents while
    // debugging. Don't write anything if this is the case.
    if (hasValue) {
      val escapeWithCData = shouldEscapeWithCData(remapped)
      if (escapeWithCData.isDefined) {
        writer.write(escapeWithCData.get)
      } else {
        val escaped = scala.xml.Utility.escape(remapped)
        writer.write(escaped)
      }
    }
  }

  override def totalElementCount = 1L

  private def remapped = XMLUtils.remapXMLIllegalCharactersToPUA(dataValueAsString)

  override def toXML(removeHidden: Boolean = true, showFormatInfo: Boolean = false): scala.xml.NodeSeq = {
    if (isHidden && removeHidden) Nil
    else {
      val elem =
        if (erd.nilledXML.isDefined && isNilled) {
          erd.nilledXML.get
        } else if (_value != null) {
          val s = remapped
          // At this point s contains only legal XML characters.
          // However, since we're going to create actual XML documents here,
          // we have to do escaping. There are two ways to do escaping.
          // One is to convert &, <, >, and " to &amp; &lt; &gt; &quot;.
          // The other is to wrap the contents in <![CDATA[ ...]]> brackets.
          // For strings longer than a certain size, or with a large number of
          // the characters requiring escaping,... CDATA is preferred.
          //
          // TODO: add some tunable option to control (a) PUA mapping or not
          // (b) CDATA or escapify, or CDATA for things of some size, or we
          // can put Daffodil specific annotations on the ERD e.g., daf:xmlEscapePolicy
          // with options for single chars, CDATA, or and generate always or
          // generate when needed. etc.
          //
          // Anyway... Constructing a Text node seems to automatically escapeify
          // the supplied content.
          val textNode = new scala.xml.Text(s)
          scala.xml.Elem(erd.thisElementsNamespacePrefix, erd.name, Null, erd.minimizedScope, true, textNode)
        } else {
          // no value yet
          scala.xml.Elem(erd.thisElementsNamespacePrefix, erd.name, Null, erd.minimizedScope, true)
        }
      val res = addFmtInfo(elem, showFormatInfo)
      res
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
  var isFinal: Boolean = false

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

  private lazy val nfe = new InfosetComplexElementNotFinalException(this)

  override def requireFinal {
    if (!isFinal) throw nfe
  }

  final override def isEmpty: Boolean = false

  final override def isNilled: Boolean = {
    if (!erd.isNillable) false
    else if (_isNilledSet) {
      _isNilled
    } else if (this.isFinal) {
      // TODO: should we check that there are no children?
      false
    } else {
      throw new InfosetNoDataException(this, erd)
    }
  }

  // the DIDocument overrides number of slots to 1.
  def nSlots = erd.nChildSlots
  protected final def slots = _slots

  private lazy val _slots = {
    val slots = new Array[DINode](nSlots); // TODO: Consider a map here. Then we'd only represent slots that get filled.
    slots
  }

  override lazy val filledSlots: IndexedSeq[DINode] = slots.filter { _ ne null }

  override def children = _slots.map { s => if (Maybe.WithNulls.isDefined(s)) Some(s) else None }.flatten.toStream

  final def getChild(erd: ElementRuntimeData): InfosetElement =
    getChild(erd.dpathElementCompileInfo)

  final def getChild(info: DPathElementCompileInfo): InfosetElement = {
    val slot = info.slotIndexInParent
    val res =
      if (slot >= slots.length) Assert.invariantFailed("slot number out of range") // null // TODO should this out-of-range be an exception?
      else {
        val s = _slots(slot)
        s.asInstanceOf[InfosetElement]
      }
    if (res ne null) res
    else {
      throw new InfosetNoSuchChildElementException(this, info)
    }
  }

  final def getChildArray(childERD: ElementRuntimeData): InfosetArray = {
    Assert.usage(childERD.isArray)
    getChildArray(childERD.dpathElementCompileInfo)
  }

  final def getChildArray(info: DPathElementCompileInfo): InfosetArray = {
    Assert.usage(info.isArray)
    val slot = info.slotIndexInParent
    getChildArray(slot)
  }

  private def getChildArray(slot: Int): InfosetArray = {
    val slotVal = _slots(slot)
    if (slotVal ne null)
      slotVal match {
        case arr: DIArray => slotVal.asInstanceOf[DIArray]
        case _ => Assert.usageError("not an array")
      }
    else {
      val arrayERD = erd.childERDs(slot)
      // slot is null. There isn't even an array object yet.
      // create one (it will have zero entries)
      val ia = new DIArray(arrayERD, this)
      // no array there yet. So we have to create one.
      setChildArray(slot, ia)
      ia
    }
  }

  final override def setChildArray(erd: ElementRuntimeData, arr: InfosetArray) {
    Assert.usage(erd.isArray)
    setChildArray(erd.slotIndexInParent, arr.asInstanceOf[DIArray])
  }

  final def setChildArray(slot: Int, arr: DIArray) {
    Assert.invariant(_slots(slot) eq null)
    _slots(slot) = arr
    _lastSlotAdded = slot
  }

  override def addChild(e: InfosetElement): Unit = {
    if (e.runtimeData.isArray) {
      //
      // make sure there is an array to accept
      // the child
      //
      val arr = getChildArray(e.runtimeData) // creates if it doesn't exist
      Assert.invariant(arr ne null)
      arr.append(e)
    } else {
      _slots(e.runtimeData.slotIndexInParent) = e.asInstanceOf[DINode]
      _lastSlotAdded = e.runtimeData.slotIndexInParent
    }
    e.setParent(this)
  }

  final override def removeHiddenElements(): InfosetElement = {
    var i = 0
    while (i < erd.nChildSlots) {
      val isH = erd.childERDs(i).isHidden
      if (isH) _slots(i) = null
      i = i + 1
    }
    this
  }

  final def captureInto(cs: DIComplexSharedImplMixin) {
    Assert.invariant(_arraySize == MaybeInt.Nope)
    Assert.invariant(cs._arraySize == MaybeInt.Nope)

    cs._arraySize =
      if (_lastSlotAdded >= 0) {
        _slots(_lastSlotAdded) match {
          case arr: DIArray => MaybeInt(arr.length.toInt)
          case _ => MaybeInt.Nope
        }
      } else {
        MaybeInt.Nope
      }
  }

  final def restoreFrom(cs: DIComplexSharedImplMixin) {
    Assert.invariant(_arraySize == MaybeInt.Nope)

    var i = _lastSlotAdded
    while (i > cs._lastSlotAdded) {
      _slots(i) = null
      i -= 1
    }
    _lastSlotAdded = cs._lastSlotAdded

    if (cs._arraySize.isDefined) {
      _slots(_lastSlotAdded).asInstanceOf[DIArray].reduceToSize(cs._arraySize.get)
    }
  }

  /**
   * Converts into XML as in a scala.xml.NodeSeq
   *
   * If there are items in the evalCache or modelGroups contained within, then
   * those are included as an XML Processing Instruction which is appended as a final child of
   * the Node making up the NodeSeq.
   */
  override def toXML(removeHidden: Boolean = true, showFormatInfo: Boolean = false): scala.xml.NodeSeq = {
    if (isHidden && removeHidden) Nil
    else {
      val elem =
        if (erd.nilledXML.isDefined && isNilled) {
          erd.nilledXML.get
        } else {
          val nonHiddenChildren = children.flatMap { slot => slot.toXML(removeHidden) }
          scala.xml.Elem(erd.thisElementsNamespacePrefix, erd.name, scala.xml.Null, erd.minimizedScope, true, nonHiddenChildren: _*)
        }
      val res = addFmtInfo(elem, showFormatInfo)
      res
    }
  }

  override def writeContents(writer: java.io.Writer, removeHidden: Boolean, indentStep: Int, indentLevel: Int) {
    _slots.foreach { slot => if (slot ne null) slot.toWriter(writer, removeHidden, indentStep, indentLevel) }
  }

  override def totalElementCount: Long = {
    if (erd.nilledXML.isDefined && isNilled) return 1L
    var a: Long = 1
    var i = 0
    while (i < _slots.length) {
      val slot = _slots(i)
      i += 1
      if (slot ne null) a += slot.totalElementCount
    }
    a
  }
}

/*
 * Making this extend DIComplex eliminates a bunch of boundary
 * conditions having to do with the document root element.
 */
final class DIDocument(erd: ElementRuntimeData) extends DIComplex(erd)
  with InfosetDocument {
  var root: DIElement = null

  /**
   * Set if this DIDocument is being attached to a DIElement just to establish the
   * invariant that there is a parent while we evaluate the expression to see if it has
   * a constant value
   */
  var isCompileExprFalseRoot: Boolean = false

  override def nSlots = 1

  def setRootElement(rootElement: InfosetElement) {
    root = rootElement.asInstanceOf[DIElement]
    addChild(root)
  }

  override def addChild(child: InfosetElement) {
    slots(0) = child.asInstanceOf[DINode]
    child.setParent(this)
    root = child.asInstanceOf[DIElement]
  }

  def getRootElement(): InfosetElement = {
    root
  }

  override def toXML(removeHidden: Boolean = true, showFormatInfo: Boolean = false) =
    if (root != null) root.toXML(removeHidden)
    else <document/>

  override def toWriter(writer: java.io.Writer, removeHidden: Boolean = true, indentStep: Int = 2, indentLevel: Int = 0) {
    if (root != null) root.toWriter(writer, removeHidden, indentStep, indentLevel)
    else writer.write("<document/>")
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

  def newDocument(root: InfosetElement): InfosetDocument = {
    val doc = newDocument(root.runtimeData)
    doc.setRootElement(root)
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
  def convertToInfosetRepType(primType: PrimType, value: String, context: ThrowsSDE): AnyRef = {
    import NodeInfo.PrimType._
    val res = primType match {
      case String => value
      case Int => value.toInt
      case Byte => value.toByte
      case Short => value.toShort
      case Long => value.toLong
      case Integer => new java.math.BigInteger(value)
      case Decimal => new java.math.BigDecimal(value)
      case UnsignedInt => {
        val res = java.lang.Long.parseLong(value)
        context.schemaDefinitionUnless(res >= 0, "Cannot convert %s to %s.", value, primType.name)
        res
      }
      case UnsignedByte => {
        val res = value.toShort
        context.schemaDefinitionUnless(res >= 0, "Cannot convert %s to %s.", value, primType.name)
        res
      }
      case UnsignedShort => {
        val res = value.toInt
        context.schemaDefinitionUnless(res >= 0, "Cannot convert %s to %s.", value, primType.name)
        res
      }
      case UnsignedLong => {
        val res = new java.math.BigInteger(value)
        context.schemaDefinitionUnless(res.doubleValue >= 0, "Cannot convert %s to %s.", value, primType.name)
        res
      }
      case NonNegativeInteger => {
        val res = new java.math.BigInteger(value)
        context.schemaDefinitionUnless(res.doubleValue >= 0, "Cannot convert %s to %s.", value, primType.name)
        res
      }
      case Double => {
        value match {
          case XMLUtils.PositiveInfinityString => scala.Double.PositiveInfinity
          case XMLUtils.NegativeInfinityString => scala.Double.NegativeInfinity
          case XMLUtils.NaNString => scala.Double.NaN
          case _ => value.toDouble
        }
      }
      case Float => {
        value match {
          case XMLUtils.PositiveInfinityString => scala.Float.PositiveInfinity
          case XMLUtils.NegativeInfinityString => scala.Float.NegativeInfinity
          case XMLUtils.NaNString => scala.Float.NaN
          case _ => value.toFloat
        }
      }
      case HexBinary => Misc.hex2Bytes(value) // convert hex constant into byte array
      case Boolean => {
        if (value == "true") true
        if (value == "false") false
        else context.schemaDefinitionError("Cannot convert %s to %s.", value, primType.name)
      }
      case Time => {
        val cal = new GregorianCalendar()
        val pos = new java.text.ParsePosition(0)
        new com.ibm.icu.text.SimpleDateFormat("HH:mm:ssZZZZZ").parse(value, cal, pos)
        DFDLTime(cal, true)
      }
      case DateTime => {
        val cal = new GregorianCalendar()
        val pos = new java.text.ParsePosition(0)
        new com.ibm.icu.text.SimpleDateFormat("uuuu-MM-dd'T'HH:mm:ssZZZZZ").parse(value, cal, pos)
        DFDLDateTime(cal, true)
      }
      case Date => {
        val cal = new GregorianCalendar()
        val pos = new java.text.ParsePosition(0)
        new com.ibm.icu.text.SimpleDateFormat("uuuu-MM-dd").parse(value, cal, pos)
        DFDLDate(cal, true)
      }
    }
    res.asInstanceOf[AnyRef]
  }

  /**
   * Used to detect arrays
   */
  @tailrec
  private[processors] def groupRuns(c: Seq[scala.xml.Node], acc: Seq[Seq[scala.xml.Node]] = Seq.empty): Seq[Seq[scala.xml.Node]] = {
    c match {
      case Seq() => acc
      case xs =>
        val (same, rest) = xs.span { _.label == xs.head.label }
        groupRuns(rest, acc :+ same)
    }
  }

  /**
   * Compute a Daffodil Infoset from a SchemaSet, and a scala XML element representing
   * the infoset (as projected into XML).
   *
   * Insures the created infoset matches the schema (rooted at that element).
   * Converts xsi:nil="true" to isNilled on the infoset
   * Converts dafint:hidden='true' to isHidden on the infoset (Daffodil extension
   * allowing testing and visualization of the Augmented Infoset)
   *
   * This is used for testing the Infoset code, but may also be useful in other places.
   * Perhaps the interactive debugger?
   */

  def elem2Infoset(erd: ElementRuntimeData, xmlElem: scala.xml.Node): InfosetElement = {
    val infosetElem = Infoset.newElement(erd)
    populate(erd, infosetElem, xmlElem)
    infosetElem
  }

  def elem2InfosetDocument(erd: ElementRuntimeData, xmlElem: scala.xml.Node): InfosetDocument = {
    val rootElem = elem2Infoset(erd, xmlElem)
    val doc = newDocument(rootElem)
    doc
  }

  def populate(erd: ElementRuntimeData,
    ie: InfosetElement,
    node: scala.xml.Node) {
    // TODO: in the future (for unparsing especially) we may want to look
    // in the scala node for dafint:valid="true" or "false", and populate
    // the isValid data member.
    ie match {
      case is: InfosetSimpleElement => populateSimple(erd, is, node)
      case ic: InfosetComplexElement => populateComplex(erd, ic, node)
    }
  }

  /**
   * Use to fill in a DISimple
   */
  def populateSimple(erd: ElementRuntimeData,
    ie: InfosetSimpleElement,
    node: scala.xml.Node) {
    val rep = valueOrNullForNil(erd, node)
    if (rep == null) ie.setNilled()
    else ie.setDataValue(rep)
  }

  /**
   *  use to fill in a DIComplex
   */
  def populateComplex(erd: ElementRuntimeData,
    ie: InfosetComplexElement,
    node: scala.xml.Node) {

    if (erd.isNillable && hasXSINilTrue(node)) {
      ie.setNilled()
      return
    }
    val runs = groupRuns(node.child.filter { _.isInstanceOf[Elem] }).map { _.toSeq }
    //
    // There is one run per slot
    //
    val erds = runs.map {
      case Seq(hd, _*) =>
        val label = hd.label
        // must ignore whitespace nodes here.
        val children = erd.childERDs.filter { _.name == label }
        if (children.isEmpty) {
          Assert.usageError("Declared element '%s' does not have child named '%s'.".format(erd.name, label))
        }
        Assert.invariant(children.length =#= 1)
        children(0)
    }
    (runs zip erds) foreach { pair =>
      pair match {
        //
        // For each slot
        //
        case (Seq(onlyOne), childERD) if (!childERD.isArray) => {
          //
          // isolated uniquely named child (not an array)
          // goes into this slot.
          //
          val childInfosetElem = elem2Infoset(childERD, onlyOne)
          ie.addChild(childInfosetElem.asInstanceOf[DIElement])
        }
        case (list, childERD) if (childERD.isArray) => {
          //
          // run of one to many identically named children
          //
          // In this case, the current slot must be filled in with
          // a DIArray
          val diComplex = ie.asInstanceOf[DIComplex]
          val arr = new DIArray(childERD, diComplex)
          val c = ie.asInstanceOf[DIComplex]
          c.setChildArray(childERD, arr)

          // now we have to populate the array

          list.map { elem =>
            val occurrenceIE = elem2Infoset(childERD, elem)
            occurrenceIE.setParent(ie)
            occurrenceIE
          }.foreach { arr.append(_) }
        }
        // FIXME: this case could happen if a DFDL schema actually has
        // two scalar elements with the same name back-to-back. That's
        // allowed in XSD, but it's not a very good idea.
        case (list, childERD) if list.length > 1 && !childERD.isArray => Assert.usageError(
          "more than one occurrence, but element is not an array")
        case _ => Assert.invariantFailed("no other cases")
      }
    }
  }

  private[processors] def valueOrNullForNil(erd: ElementRuntimeData, node: scala.xml.Node): AnyRef = {
    Assert.usage(erd.isSimpleType)
    val primType = erd.optPrimType.get
    val rep =
      if (erd.isNillable && hasXSINilTrue(node)) null
      else {
        // the .text method removes XML escaping.
        // so if the node has &amp; in it, an & character will be produced.
        // Also if the node has <![CDATA[...]]> it will be removed.
        // (Different XML Loader may have converted the CDATA into a
        // scala.xml.PCData node, or may have converted it into
        // escapified text. Either way the .text method gets us
        // to "real" data)
        // The .text method similarly concatenates all the children of
        // a node. Wierd that node.child produces a NodeSeq of children
        // which are Text or PCData or ...? nodes. But .text does the
        // right thing with them.
        val value = node.child.text
        val remapped = XMLUtils.remapPUAToXMLIllegalCharacters(value)
        convertToInfosetRepType(primType, remapped, erd)
      }
    rep
  }

  /**
   * Returns true if the node has xsi:nil="true" attribute.
   *
   * Does not require the xsi namespace to be defined.
   */
  private[processors] def hasXSINilTrue(node: scala.xml.Node): Boolean = {
    val attribsList = if (node.attributes == null) Nil else node.attributes

    val res = attribsList.exists { (attribute: MetaData) =>
      {
        val name = attribute.key
        val value = attribute.value.text
        val prefixedKey = attribute.prefixedKey
        val prefix = if (prefixedKey.contains(":")) prefixedKey.split(":")(0) else ""
        val hasXSINil = (prefix == "xsi" && name == "nil")
        val res = hasXSINil && value == "true"
        res
      }
    }
    res
  }

}
