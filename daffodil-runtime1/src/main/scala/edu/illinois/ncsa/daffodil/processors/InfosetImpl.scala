package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
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
import edu.illinois.ncsa.daffodil.dsom.ImplementsThrowsSDE
import edu.illinois.ncsa.daffodil.xml.NoNamespace
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType

sealed trait DINode extends {
  def toXML: scala.xml.NodeSeq
}

/**
 * Exception thrown if infoset doesn't have a child corresponding to the
 * slot being probed. E.g., expression evaluation reaching to a forward
 * sibling that has not yet been parsed.
 */
class InfosetNoSuchChildElementException(msg: String) extends ProcessingError {
  def this(erd: ElementRuntimeData) = this("Child element %s does not exist.".format(erd.prettyName))
  // def this(slot: Int) = this("Child element with slot position %s does not exist.".format(slot))
  def this(name: String, namespace: NS, slot: Int) = this("Child named '" + name +
    (if (namespace == NoNamespace) "' " else "' in namespace '%s' ".format(namespace)) +
    "(slot " + slot + ") does not exist.")

  override def getMessage: String = msg
}

class InfosetArrayIndexOutOfBoundsException(msg: String) extends ProcessingError {
  def this(name: String, namespace: NS, slot: Long, length: Long) = {
    this("Value %d is out of range for the '%s' array%swith length %d".format(
      slot,
      name,
      if (namespace == NoNamespace) " " else " in namespace '%s' ".format(namespace),
      length))
  }

  override def getMessage: String = msg
}

/**
 * Used to determine if expressions can be evaluated without any nodes.
 * They all save/restore the current node, so this is the placeholder
 * they use for that purpose.
 */
final class FakeDINode extends DISimple(null) {
  private def die = throw new java.lang.IllegalStateException("No infoset at compile time.")

  override def toXML: scala.xml.NodeSeq = die
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

  private var _value: Any = null
  override def dataValue: Any = _value
  override def setDataValue(s: Any): Unit = { _value = s }

  override def dataValueAsString: String = _value.toString
  override def isDefaulted: Boolean = die
}

/**
 * Base for non-array elements. That is either scalar or optional (
 * minOccurs 0, maxOccurs 1)
 */
sealed trait DIElement extends DINode with InfosetElement {
  protected def erd: ElementRuntimeData
  override final def name: String = erd.name
  override final def namespace: NS = erd.targetNamespace

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
  protected final var _parent: Maybe[InfosetComplexElement] = Nope
  protected final var _isNilled: Boolean = false
  protected final var _validity: Maybe[Boolean] = Nope

  override def parent = _parent
  def diParent = _parent.asInstanceOf[Maybe[DIComplex]]
  override def setParent(p: InfosetComplexElement) {
    _parent = One(p)
  }

  override def isNilled: Boolean = _isNilled
  override def setNilled(): Unit = {
    Assert.invariant(erd.isNillable)
    _isNilled = true
  }

  /**
   * valid = Nope means not checked
   * valid = One(true) means valid
   * valid = One(false) means invalid
   */
  override def valid = _validity
  override def setValid(validity: Boolean) { _validity = One(validity) }

}

// This is not a mutable collection class on purpose.
// This forces use of while-loops and similar known-efficient
// code, rather than letting all sorts of map/flatmap compositions,
// which may or may not be optimized effectively.
//
final class DIArray(name: String, namespace: NS) extends DINode with InfosetArray {
  private val initialSize = DaffodilTunableParameters.initialElementOccurrencesHint.toInt
  //TODO: really this needs to be adaptive, and resize upwards reasonably. 
  //A non-copying thing - list like, may be better, but we do need access to be
  //constant time. 
  final protected val _contents = new ArrayBuffer[InfosetElement](initialSize)

  /**
   * Used to shorten array when backtracking out of having appended elements.
   */
  def trimEnd(n: Int) {
    _contents.trimEnd(n)
  }

  def getOccurrence(occursIndex: Long) = {
    if (occursIndex > length || occursIndex < 1) throw new InfosetArrayIndexOutOfBoundsException(name, namespace, occursIndex, length)
    _contents(occursIndex.toInt - 1)
  }

  def apply(occursIndex: Long) = getOccurrence(occursIndex)

  def append(ie: InfosetElement): Unit = {
    _contents.append(ie)
  }

  final def length: Long = _contents.length

  final def toXML: scala.xml.NodeSeq = {
    _contents.flatMap { _.toXML }
  }
}

sealed class DISimple(val erd: ElementRuntimeData)
  extends DIElement
  with InfosetSimpleElement {
  protected var _isDefaulted: Boolean = false

  private var _value: Any = null

  override def setDataValue(x: Any) {
    Assert.invariant(x != null)
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
    _validity = Nope // we have not tested this new value. 
    _value = x
  }

  /**
   * Obtain the data value. Implements default
   * values.
   */
  override def dataValue = {
    if (_value == null && erd.isDefaultable) {
      val defaultVal = erd.defaultValue.get
      _value = defaultVal
      _isDefaulted = true
    }
    this.erd.schemaDefinitionUnless(_value != null, "Value has not been set.")
    _value
  }

  override def dataValueAsString = {
    dataValue match {
      case s: String => s
      case arr: Array[Byte] => Misc.bytes2Hex(arr)
      case d: Double => {
        //
        // Print these as needed in XML/XSD
        //
        if (d == Double.PositiveInfinity) "INF"
        else if (d == Double.NegativeInfinity) "-INF"
        else d.toString
      }
      case f: Float => {
        if (f == Float.PositiveInfinity) "INF"
        else if (f == Float.NegativeInfinity) "-INF"
        else f.toString
      }
      case _ => dataValue.toString
    }
  } // TODO: caching these strings

  override def isDefaulted: Boolean = {
    dataValue // access this for side-effect that checks for default value.
    _isDefaulted
  }

  override def removeHiddenElements(): InfosetElement = this

  override def toXML: scala.xml.NodeSeq = {
    if (isHidden) Nil //TODO: provide control over whether to display these or not.
    else {
      val elem =
        if (erd.nilledXML.isDefined && isNilled) {
          erd.nilledXML.get
        } else if (_value != null) {
          val s = XMLUtils.remapXMLIllegalCharactersToPUA(dataValueAsString)
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
          scala.xml.Elem(erd.thisElementsNamespacePrefix, erd.name, Null, erd.namespaces, true, textNode)
        } else {
          // no value yet
          scala.xml.Elem(erd.thisElementsNamespacePrefix, erd.name, Null, erd.namespaces, true)
        }
      elem
    }
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
    var validity: Maybe[Boolean],
    var value: Any) extends InfosetElementState

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
sealed class DIComplex(val erd: ElementRuntimeData)
  extends DIElement with InfosetComplexElement {

  // the DIDocument overrides number of slots to 1. 
  def nSlots = erd.nChildSlots
  final protected def slots = _slots

  private lazy val _slots = {
    val slots = new Array[Maybe[DINode]](nSlots);

    // initialize slots to Nope
    var i = 0
    while (i < slots.length) {
      slots(i) = Nope
      i = i + 1
    }
    slots
  }

  final override def getChild(erd: ElementRuntimeData): InfosetElement = getChildMaybe(erd).getOrElse {
    throw new InfosetNoSuchChildElementException(erd)
  }

  final override def getChildMaybe(erd: ElementRuntimeData): Maybe[InfosetElement] =
    getChildMaybe(erd.slotIndexInParent)

  final override def getChildArray(erd: ElementRuntimeData): Maybe[InfosetArray] = {
    Assert.usage(erd.isArray)
    getChildArray(erd.slotIndexInParent, erd.prettyName, erd.targetNamespace)
  }

  final def getChild(slot: Int, name: String, namespace: NS): InfosetElement = getChildMaybe(slot).getOrElse {
    throw new InfosetNoSuchChildElementException(name, namespace, slot)
  }

  final def getChildMaybe(slot: Int): Maybe[InfosetElement] =
    _slots(slot).map { _.asInstanceOf[InfosetElement] }

  final def getChildArray(slot: Int, name: String, namespace: NS): Maybe[DIArray] = {
    val slotVal = _slots(slot)
    if (slotVal.isDefined)
      slotVal.get match {
        case arr: DIArray => slotVal.asInstanceOf[Maybe[DIArray]]
        case _ => Assert.usageError("not an array")
      }
    else {
      // slot is Nope. There isn't even an array object yet.
      // create one (it will have zero entries)
      val ia = One(new DIArray(name, namespace))
      // no array there yet. So we have to create one.
      setChildArray(slot, ia)
      ia
    }
  }

  final override def setChildArray(erd: ElementRuntimeData, arr: InfosetArray) {
    setChildArray(erd.slotIndexInParent, One(arr.asInstanceOf[DIArray]))
  }

  final def setChildArray(slot: Int, arr: Maybe[DIArray]) {
    _slots(slot) = arr
  }

  override def addChild(e: InfosetElement): Unit = {
    if (e.runtimeData.isArray) {
      //
      // make sure there is an array to accept 
      // the child 
      var ia: InfosetArray = null
      val arr = getChildArray(e.runtimeData)
      if (!arr.isDefined) {
        ia = new DIArray(e.runtimeData.name, e.runtimeData.targetNamespace)
        // no array there yet. So we have to create one.
        setChildArray(e.runtimeData, ia)
      } else {
        ia = arr.get
      }
      // At this point there IS an array
      ia.append(e)
    } else {
      _slots(e.runtimeData.slotIndexInParent) = One(e.asInstanceOf[DINode])
    }
    e.setParent(this)
  }

  final override def removeChild(e: InfosetElement): Unit = {
    _slots(e.runtimeData.slotIndexInParent) = Nope
  }

  final override def removeHiddenElements(): InfosetElement = {
    var i = 0
    while (i < erd.nChildSlots) {
      val isH = erd.childERDs(i).isHidden
      if (isH) _slots(i) = Nope
      i = i + 1
    }
    this
  }

  //TODO: make these use a thread-local pool so that we avoid allocating
  //these objects that obey a stack discipline.
  final override def captureState(): InfosetElementState =
    DIComplexState(_isNilled, _validity, _slots)

  final override def restoreState(st: InfosetElementState): Unit = {
    val ss = st.asInstanceOf[DIComplexState]
    _isNilled = ss.isNilled
    _validity = ss.validity
    val si = ss.slotsInfo
    var i = 0
    while (i < si.length) {
      si(i) match {
        case DIComplexState.NOT_DEFINED => _slots(i) = Nope
        case DIComplexState.DEFINED_ONEONLY => {
          Assert.invariant(_slots(i).isDefined)
          // TODO: is this ok? Do we have to know what the value in fact was?
          // Or is it sufficient to just know it had a value and still does.
        }
        case arrayLength => {
          val arr = _slots(i).get.asInstanceOf[DIArray]
          if (arr.length > arrayLength) {
            // 
            // we must shorten the array. 
            // 
            arr.trimEnd(arr.length.toInt - arrayLength)
          }
        }
      }
      i = i + 1
    }
  }

  class DIComplexState(val isNilled: Boolean, val validity: Maybe[Boolean], val slotsInfo: Array[Int])
    extends InfosetElementState
  object DIComplexState {

    val NOT_DEFINED = -1
    val DEFINED_ONEONLY = -2

    /**
     * SlotsInfo is an array of Int that is a summary of the state
     * of a complex element. We represent only whether the slot was
     * occupied, and how long an array was, but that is all.
     * If -1 then the actual slot was Nope
     * If -2 then the actual slot was One(IE) for a non-array IE
     * if N >=0, then the slot was One(Arr) and Arr was of length N.
     */
    def slotsInfo(slots: Array[Maybe[DINode]]) = {
      val slotsInfo = new Array[Int](slots.length)
      var i = 0
      val NOT_DEFINED = -1
      val DEFINED_ONEONLY = -2
      //  non-negative values mean an array with N children
      while (i < slots.length) {
        slotsInfo(i) =
          if (!slots(i).isDefined) NOT_DEFINED
          else {
            val ie = slots(i).get
            ie match {
              case ab: DIArray => ab.length.toInt
              case _ => DEFINED_ONEONLY
            }
          }
        i = i + 1
      }
      slotsInfo
    }

    def apply(isNilled: Boolean, validity: Maybe[Boolean], slots: Array[Maybe[DINode]]) =
      new DIComplexState(isNilled, validity, slotsInfo(slots))
  }

  override def toXML: scala.xml.NodeSeq = {
    if (isHidden) Nil
    else {
      val elem =
        if (erd.nilledXML.isDefined && isNilled) {
          erd.nilledXML.get
        } else {
          val children = _slots.flatMap { _ map { slot => slot.toXML } }.toSeq.flatten
          scala.xml.Elem(erd.thisElementsNamespacePrefix, erd.name, scala.xml.Null, erd.namespaces, true, children: _*)
        }
      elem
    }
  }
}

/*
 * Making this extend DIComplex eliminates a bunch of boundary 
 * conditions having to do with the document root element. 
 */
final class DIDocument(erd: ElementRuntimeData) extends DIComplex(erd)
  with InfosetDocument {
  private var root: DIElement = null

  override def nSlots = 1

  def setRootElement(rootElement: InfosetElement) {
    root = rootElement.asInstanceOf[DIElement]
    addChild(root)
  }

  override def addChild(child: InfosetElement) {
    slots(0) = One(child.asInstanceOf[DINode])
    child.setParent(this)
    root = child.asInstanceOf[DIElement]
  }

  def getRootElement(): InfosetElement = {
    root
  }

  override def toXML =
    if (root != null) root.toXML
    else <document/>
}

object Infoset {

  import NodeInfo.PrimType._

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
      case Double => value.toDouble
      case Float => value.toFloat
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
    val runs = groupRuns(node.child).map { _.toSeq }
    //
    // There is one run per slot
    //
    val erds = runs.map {
      case Seq(hd, _*) =>
        val label = hd.label
        val childERD = erd.childERDs.find { _.name == label }.getOrElse(
          Assert.usageError(
            "Declared element '%s' does not have child named '%s'.".format(erd.name, label)))
        childERD
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
          val arr = new DIArray(childERD.prettyName, childERD.targetNamespace)
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

  private[processors] def valueOrNullForNil(erd: ElementRuntimeData, node: scala.xml.Node): Any = {
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
        //
        // The only thing left is we want to invert our illegal-XML characters
        // from the PUA to their regular character values. E.g., &#xE000; produces
        // unicode U+E000, but we want U+0000 (aka NUL). 
        //
        // FIXME: This needs to invert the PUA mapping (unless a tunable says
        // not to)
        val value = node.child.text
        convertToInfosetRepType(primType, value, erd)
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
