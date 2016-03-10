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

import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.util.MaybeInt
import scala.collection.mutable.ArrayBuffer
import edu.illinois.ncsa.daffodil.exceptions.Assert
import com.ibm.icu.util.Calendar
import edu.illinois.ncsa.daffodil.xml.NS
import scala.xml.PrefixedAttribute
import com.ibm.icu.util.GregorianCalendar
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocatable
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import scala.xml.Elem
import scala.xml.NamespaceBinding
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import scala.xml.MetaData
import scala.annotation.tailrec
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.util.Misc
import scala.xml.Null
import scala.util.DynamicVariable
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.xml.NoNamespace
import edu.illinois.ncsa.daffodil.xml.NamedQName
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.dpath.DState
import edu.illinois.ncsa.daffodil.dsom.DPathElementCompileInfo
import edu.illinois.ncsa.daffodil.dsom.DiagnosticImplMixin
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.exceptions.ThinThrowable
import edu.illinois.ncsa.daffodil.util.MaybeBoolean
import scala.collection.IndexedSeq
import scala.collection.JavaConversions._
import edu.illinois.ncsa.daffodil.dpath.AsIntConverters._
import edu.illinois.ncsa.daffodil.api.Diagnostic

sealed trait DINode {
  def toXML(removeHidden: Boolean = true, showFormatInfo: Boolean = false): scala.xml.NodeSeq

  def asSimple: DISimple = {
    this match {
      case diSimple: DISimple => diSimple
      case _ =>
        throw new InfosetWrongNodeType(this) // see comment with exception class definition for why this can happen
    }
  }

  def asComplex: DIComplex = {
    this match {
      case diComplex: DIComplex => diComplex
      case _ =>
        throw new InfosetWrongNodeType(this) // see comment with exception class definition for why this can happen
    }
  }

  def children: Stream[DINode]
  def toWriter(writer: java.io.Writer, removeHidden: Boolean = true): Unit
  def totalElementCount: Long
  def namedQName: NamedQName
  def erd: ElementRuntimeData

  /**
   * Can treat any DINode, even simple ones, as a container of other nodes.
   * This simplifies walking an infoset.
   */
  def filledSlots: IndexedSeq[DINode]
  final def numChildren = filledSlots.length

  /*
   * Unparser specific things
   */
  /**
   * Indicates that no more children can be added to this node
   * and that its value will not be set if it isn't already here.
   *
   * This transitions to true once an End(Node) Infoset event has
   * been processed.
   *
   * If this is true, then expressions referencing this node never
   * need to suspend. Any suspended expressions at the time this
   * is set to true, become unsuspended and are evaluated.
   *
   * That may result in success or failure of the expression. E.g.,
   * an expression that is pending on traversing to a child with a particular
   * ERD, if the node is closed without that child being added, then
   * the expression will fail.
   */
  private var _isClosed: Boolean = false
  def setClosed() {
    _isClosed = true
  }
  def isClosed: Boolean = _isClosed

}

trait InfosetException extends DiagnosticImplMixin with ThinThrowable

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
 */
class InfosetWrongNodeType(val node: DINode)
  extends ProcessingError("Error", Nope, Nope, "Wrong type (simple when complex expected or vice versa)", node)
  with InfosetException

/**
 * Exception thrown if infoset doesn't have a child corresponding to the
 * slot being probed. E.g., expression evaluation reaching to a forward
 * sibling that has not yet been parsed.
 */
class InfosetNoSuchChildElementException(val diComplex: DIComplex, val info: DPathElementCompileInfo)
  extends ProcessingError("Error", Nope, Nope, "Child element %s does not exist.", info.namedQName)
  with InfosetException

class InfosetNoInfosetException(val rd: Maybe[RuntimeData])
  extends ProcessingError("Error", Nope, Nope, "There is no infoset%s", (if (rd.isEmpty) "." else " for path %s.".format(rd.get.path)))
  with InfosetException

class InfosetNoDataException(val diSimple: DISimple, val erd: ElementRuntimeData)
  extends ProcessingError("Error", Nope, Nope, "Element %s does not have a value.", erd.namedQName)
  with InfosetException

class InfosetArrayIndexOutOfBoundsException(val diArray: DIArray, val index: Long, val length: Long)
  extends ProcessingError("Error", Nope, Nope, "Value %d is out of range for the '%s' array with length %d", index, diArray.erd.namedQName, length)
  with InfosetException

class InfosetNoRootException(val diElement: DIElement, val erd: ElementRuntimeData)
  extends ProcessingError("Error", Nope, Nope, "No root element reachable from element %s.", erd.namedQName)
  with InfosetException

class InfosetNoParentException(val diElement: DIElement, val erd: ElementRuntimeData)
  extends ProcessingError("Error", Nope, Nope, "No parent element for element %s.", erd.namedQName)
  with InfosetException

/**
 * Used to determine if expressions can be evaluated without any nodes.
 * They all save/restore the current node, so this is the placeholder
 * they use for that purpose.
 */
final class FakeDINode extends DISimple(null) {
  private def die = throw new java.lang.IllegalStateException("No infoset at compile time.")

  override def toXML(removeHidden: Boolean = true, showFormatInfo: Boolean = false): scala.xml.NodeSeq = die
  override def captureState(): InfosetElementState = die
  override def removeHiddenElements(): InfosetElement = die
  override def restoreState(state: InfosetElementState): Unit = die

  override def parent = die
  override def diParent = die
  override def setParent(p: InfosetComplexElement): Unit = die

  override def isNilled: Boolean = die
  override def setNilled(): Unit = die

  override def valid = die
  override def setValid(validity: Boolean): Unit = die

  private var _value: AnyRef = null
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

  def parentTerm: DITerm

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
  protected def fmtInfo: Maybe[String]

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

private[processors] sealed trait HasModelGroupMixin { self: DITerm =>

  final lazy val modelGroups: java.util.Map[ModelGroupRuntimeData, DIModelGroup] = new java.util.LinkedHashMap[ModelGroupRuntimeData, DIModelGroup]

  def modelGroup(mgrd: ModelGroupRuntimeData): DIModelGroup = {
    val got = modelGroups.get(mgrd)
    val tNode = if (got ne null) got
    else {
      val newTNode = mgrd match {
        case srd: SequenceRuntimeData => new DISequence(srd, self)
        case crd: ChoiceRuntimeData => new DIChoice(crd, self)
      }
      modelGroups.put(mgrd, newTNode)
      newTNode
    }
    tNode
  }

  protected final def modelGroupsPseudoXML: String = {
    val pxml = mapAsScalaMap(modelGroups).map {
      case (_, mg) =>
        mg.toPseudoXML()
    }
    pxml.mkString("\n")
  }

  protected def fmtInfo = {
    val mgXML = modelGroupsPseudoXML
    val pecXML = parserEvalCache.toPseudoXML()
    val uecXML = unparserEvalCache.toPseudoXML()
    val puxml = {
      mgXML +
        (if (pecXML =:= "") "" else "\n" + pecXML) +
        (if (uecXML =:= "") "" else "\n" + uecXML)
    }
    Maybe(if (puxml =:= "") null else puxml)
  }
}

sealed abstract class DIModelGroup(val mgrd: ModelGroupRuntimeData, val parentTerm: DITerm) extends DITerm with HasModelGroupMixin {
  override final def trd = mgrd

  /**
   * Name for use in pseudo-xml representation
   */
  protected def nom: String

  def toPseudoXML(): String = {
    val relPath = trd.dpathCompileInfo.relativePath
    val pecXML = parserEvalCache.toPseudoXML()
    val uecXML = unparserEvalCache.toPseudoXML()
    val ecXMLString = (if (pecXML =:= "") "" else "\n" + pecXML) + (if (uecXML =:= "") "" else "\n" + uecXML)
    "<" + dafPrefix + ":" + nom + " " + "path=\"" + relPath + "\"" + ecXMLString + ">"
  }
}

sealed class DISequence(srd: SequenceRuntimeData, parentTerm: DITerm) extends DIModelGroup(srd, parentTerm) {
  /**
   * Name for use in pseudo-xml representation.
   *
   * Could have been derived from the class name, but only at the cost of allocating strings.
   *
   * Converting to XML, even though this stuff is only "for debug", the overhead still
   * matters to some extent.
   */
  override final def nom = "sequence"
}

sealed class DIChoice(crd: ChoiceRuntimeData, parentTerm: DITerm) extends DIModelGroup(crd, parentTerm) {

  /**
   * See DISequence.nom
   */
  override final def nom = "choice"
}

/**
 * Base for non-array elements. That is either scalar or optional (
 * minOccurs 0, maxOccurs 1)
 */
sealed trait DIElement extends DINode with DITerm with InfosetElement {
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
    cl + "(name='" + n + "')"
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

  def parentTerm: DITerm = {
    if (parentModelGroup.isDefined) parentModelGroup.get
    else diParent
  }

  def parentModelGroup: Maybe[DIModelGroup] = {
    val trd = erd.immediateEnclosingTermRuntimeData.get
    val pt = trd match {
      case mg: ModelGroupRuntimeData => One(diParent.modelGroup(mg)) // elements always appear within a model group
      case _ => Nope // exception for the root element which appears inside the DIDocument, which is not a model group
    }
    pt
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
  protected final var _isNilled: Boolean = false
  protected final var _validity: MaybeBoolean = MaybeBoolean.Nope

  override def parent = _parent
  def diParent = _parent.asInstanceOf[DIComplex]
  override def setParent(p: InfosetComplexElement) {
    Assert.invariant(_parent eq null)
    _parent = p
  }

  override def isNilled: Boolean = _isNilled
  override def setNilled(): Unit = {
    Assert.invariant(erd.isNillable)
    Assert.invariant(!_isNilled)
    _isNilled = true
  }

  /**
   * valid = Nope means not checked
   * valid = One(true) means valid
   * valid = One(false) means invalid
   */
  override def valid = _validity
  override def setValid(validity: Boolean) { _validity = MaybeBoolean(validity) }

  private def pre = erd.thisElementsNamespacePrefix
  private lazy val qn = if (pre == null | pre == "") erd.name else pre + ":" + erd.name
  protected final lazy val nilledTag = "<" + qn + erd.uniqueScope.toString + " xsi:nil=\"true\" />"
  protected final lazy val startTag = "<" + qn + erd.uniqueScope.toString + ">"
  protected final lazy val endTag = "</" + qn + ">"

  protected def writeContents(writer: java.io.Writer, removeHidden: Boolean): Unit

  override def toWriter(writer: java.io.Writer, removeHidden: Boolean = true) {
    if (isHidden && removeHidden) return
    if (isNilled) {
      writer.write(nilledTag)
    } else {
      writer.write(startTag)
      writeContents(writer, removeHidden)
      writer.write(endTag)
    }
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
  extends DINode with InfosetArray {

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
    if (occursIndex1b > length || occursIndex1b < 1)
      throw new InfosetArrayIndexOutOfBoundsException(this, occursIndex1b, length)
    _contents(occursIndex1b.toInt - 1)
  }

  @inline final def apply(occursIndex1b: Long) = getOccurrence(occursIndex1b)

  def append(ie: InfosetElement): Unit = {
    _contents += ie.asInstanceOf[DIElement]
  }

  final def length: Long = _contents.length

  final def toXML(removeHidden: Boolean = true, showFormatInfo: Boolean = false): scala.xml.NodeSeq = {
    _contents.flatMap { _.toXML(removeHidden, showFormatInfo) }
  }

  final def toWriter(writer: java.io.Writer, removeHidden: Boolean = true) {
    _contents.foreach { _.toWriter(writer, removeHidden) }
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
case class OutputValueCalcEvaluationException(val diSimple: DISimple)
  extends Exception with ThinThrowable with DiagnosticImplMixin

sealed class DISimple(override val erd: ElementRuntimeData)
  extends DIElement
  with InfosetSimpleElement {

  def filledSlots: IndexedSeq[DINode] = IndexedSeq.empty

  protected var _isDefaulted: Boolean = false

  private var _value: AnyRef = null

  override def children: Stream[DINode] = Stream.Empty

  def setDefaultedDataValue(defaultedValue: AnyRef) = {
    setDataValue(defaultedValue)
    _isDefaulted = true
  }

  override def setNilled() {
    Assert.invariant(!hasValue)
    _isNilled = true
  }

  override def setDataValue(x: AnyRef) {
    Assert.invariant(!hasValue)
    overwriteDataValue(x) // conversions for text numbers displace the string with the number.
  }

  def overwriteDataValue(x: AnyRef) {
    Assert.invariant(!x.isInstanceOf[(Any, Any)]) // legacy code created these pairs. TODO: Remove?
    //
    // let's find places where we're putting a string in the infoset
    // but the simple type is not string.
    //
    val nodeKind = erd.optPrimType.get
    if (x.isInstanceOf[String]) {
      if (!nodeKind.isInstanceOf[NodeInfo.String.Kind]) {
        // This is normal behavior for text numbers. First we parse the string
        // based on length rules and/or delimiters
        // then we run a quasi-parser which takes the value and converts
        // it by parsing it as a textual number. It then overwrites
        // the value with the value of the number type.
        //
        // This comment code block is here because it is such a useful
        // place to put a breakpoint for debugging.
        //
        // TODO: chase down places that a string is being put in the infoset
        // but as the representation of a type, not as a temporary thing
        // that is to be immediately converted to the 'real' type by the
        // next parser.
        // println("assigning a string where " + nodeKind + " is required.")
      }
    }
    _isNilled = false
    _isDefaulted = false
    _validity = MaybeBoolean.Nope // we have not tested this new value.
    _value = asAnyRef(x)
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
  } // TODO: caching these strings

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

  override final def writeContents(writer: java.io.Writer, removeHidden: Boolean) {
    val escapeWithCData = shouldEscapeWithCData(remapped)
    if (escapeWithCData.isDefined) {
      writer.write(escapeWithCData.get)
    } else {
      val escaped = scala.xml.Utility.escape(remapped)
      writer.write(escaped)
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

  protected def fmtInfo = {
    val pecXML = parserEvalCache.toPseudoXML()
    val uecXML = unparserEvalCache.toPseudoXML()
    val str =
      pecXML + (if (pecXML !=:= "") "\n" else "") + (if (uecXML =:= "") "" else "\n" + uecXML)
    val res =
      if (str =:= "") Nope else Maybe(str)
    res
  }

  //TODO: make these use a pool of these DISimpleState objects
  // so as to avoid allocating and discarding when really we only need
  // a handful of them and they obey a stack discipline.
  //
  override def captureState(): InfosetElementState =
    DISimpleState(_isNilled, _isDefaulted, _validity, _value)
  override def restoreState(st: InfosetElementState): Unit = {
    val ss = st.asInstanceOf[DISimpleState]
    _isNilled = ss.isNilled
    _validity = ss.validity
    _isDefaulted = ss.isDefaulted
    _value = ss.value
  }

  case class DISimpleState(var isNilled: Boolean,
    var isDefaulted: Boolean,
    var validity: MaybeBoolean,
    var value: AnyRef) extends InfosetElementState

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
  extends DIElement with InfosetComplexElement
  with HasModelGroupMixin {

  final override def isEmpty: Boolean = false

  // the DIDocument overrides number of slots to 1.
  def nSlots = erd.nChildSlots
  protected final def slots = _slots

  private lazy val _slots = {
    val slots = new Array[DINode](nSlots); // TODO: Consider a map here. Then we'd only represent slots that get filled.
    slots
  }

  private var _lastSlotAdded = -1

  override lazy val filledSlots: IndexedSeq[DINode] = slots.filter { _ ne null }

  override def children = _slots.map { s => if (Maybe.WithNulls.isDefined(s)) Some(s) else None }.flatten.toStream

  final override def getChild(erd: ElementRuntimeData): InfosetElement = getChild(erd.dpathElementCompileInfo)

  final def getChild(info: DPathElementCompileInfo): InfosetElement = {
    val res = getChildMaybe(info)
    if (res ne null) res
    else {
      throw new InfosetNoSuchChildElementException(this, info)
    }
  }

  final override def getChildMaybe(erd: ElementRuntimeData): InfosetElement =
    getChildMaybe(erd.dpathElementCompileInfo)

  final def getChildMaybe(info: DPathElementCompileInfo): InfosetElement = {
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

  //TODO: make these use a thread-local pool so that we avoid allocating
  //these objects that obey a stack discipline.
  final override def captureState(): InfosetElementState = {
    val arrSize =
      if (_lastSlotAdded >= 0) {
        _slots(_lastSlotAdded) match {
          case arr: DIArray => MaybeInt(arr.length.toInt)
          case _ => MaybeInt.Nope
        }
      } else {
        MaybeInt.Nope
      }
    new DIComplexState(_isNilled, _validity, _lastSlotAdded, arrSize)
  }

  final override def restoreState(st: InfosetElementState): Unit = {
    val ss = st.asInstanceOf[DIComplexState]
    _isNilled = ss.isNilled
    _validity = ss.validity

    var i = _lastSlotAdded
    while (i > ss.lastSlotAdded) {
      _slots(i) = null
      i -= 1
    }
    _lastSlotAdded = ss.lastSlotAdded

    if (ss.arraySize.isDefined) {
      _slots(_lastSlotAdded).asInstanceOf[DIArray].reduceToSize(ss.arraySize.get)
    }
  }

  class DIComplexState(val isNilled: Boolean, val validity: MaybeBoolean, val lastSlotAdded: Int, val arraySize: MaybeInt)
    extends InfosetElementState

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

  override def writeContents(writer: java.io.Writer, removeHidden: Boolean) {
    _slots.foreach { slot => if (slot ne null) slot.toWriter(writer, removeHidden) }
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

  override def toWriter(writer: java.io.Writer, removeHidden: Boolean = true) {
    if (root != null) root.toWriter(writer, removeHidden)
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
        cal
      }
      case DateTime => {
        val cal = new GregorianCalendar()
        val pos = new java.text.ParsePosition(0)
        new com.ibm.icu.text.SimpleDateFormat("uuuu-MM-dd'T'HH:mm:ssZZZZZ").parse(value, cal, pos)
        cal
      }
      case Date => {
        val cal = new GregorianCalendar()
        val pos = new java.text.ParsePosition(0)
        new com.ibm.icu.text.SimpleDateFormat("uuuu-MM-dd").parse(value, cal, pos)
        cal
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
