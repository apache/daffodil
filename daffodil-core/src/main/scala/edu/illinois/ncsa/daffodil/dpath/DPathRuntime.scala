package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.processors._
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.xml.RefQName
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util.OnStack
import edu.illinois.ncsa.daffodil.util.PreSerialization
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.Calendar
import scala.math.BigDecimal.RoundingMode
import edu.illinois.ncsa.daffodil.util.Bits
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import java.text.ParsePosition
import com.ibm.icu.util.DFDLCalendar
import com.ibm.icu.util.SimpleTimeZone
import com.ibm.icu.util.TimeZone
import java.nio.ByteBuffer
import com.ibm.icu.util.DFDLDateTime
import com.ibm.icu.util.DFDLDate
import com.ibm.icu.util.DFDLTime

import AsIntConverters._

class CompiledDPath(val ops: RecipeOp*) extends Serializable {

  def this(ops: List[RecipeOp]) = this(ops.toArray: _*)

  override def toString = toXML.toString

  def toXML = <CompiledDPath>{ ops.map { _.toXML } }</CompiledDPath>

  def runExpression(pstate: PState) {
    val dstate = pstate.dstate
    dstate.setCurrentNode(pstate.infoset.asInstanceOf[DINode])
    dstate.setVMap(pstate.variableMap)
    dstate.setPState(pstate)
    dstate.resetValue
    run(dstate)
  }

  /**
   * Used at compilation time to evaluate expressions to determine
   * if they are constant valued.
   *
   * TODO: constnat folding really should operate on sub-expressions of expressions
   * so that part of an expression can be constant, not necessarily the whole thing.
   */
  def runExpressionForConstant(sfl: SchemaFileLocation): Option[Any] = {

    //
    // we use a special dummy dstate here that errors out via throw
    // if the evaluation tries to get a processor state or node.
    //
    val dstate = new DStateForConstantFolding
    val isConstant: Boolean =
      try {
        run(dstate)
        // it ran, so must have produced a constant value
        val v = dstate.currentValue
        Assert.invariant(v != null)
        // the only way dstate can have a value is if setCurrentValue was called
        // so this is redundant. Remove?
        // dstate.setCurrentValue(v) // side effect nulls out the current node
        true
      } catch {
        // 
        // We use IllegalStateException to indicate that the DState was manipulated 
        // in a way that is not consistent with a constant expression. Such as trying to do 
        // anything with the infoset other than saving and restoring current position in the infoset. 
        case e: java.lang.IllegalStateException =>
          false // useful place for breakpoint
        // if the expression is all literals, but illegal such as xs:int("foobar") then 
        // all the pieces are constant, but evaluating will throw NumberFormatException
        // or dfdl:length='{ 5 / 0 }' - contrived yes, but in larger expressions misakes like this
        // are typically typographical errors so it is good to pick them up here.
        case e: java.lang.NumberFormatException => throw new SchemaDefinitionError(Some(sfl), None, e.getMessage)
        case e: java.lang.IndexOutOfBoundsException => false
        case e: java.lang.IllegalArgumentException => false
        case e: SchemaDefinitionDiagnosticBase => throw new SchemaDefinitionError(Some(sfl), None, e.getMessage)
        case e: ProcessingError => throw new SchemaDefinitionError(Some(sfl), None, e.getMessage)
      }
    val res =
      if (isConstant) Some(dstate.currentValue) else None
    res
  }

  def run(dstate: DState) {
    var i = 0
    // Assert.invariant(ops.length > 0)
    while (i < ops.length) {
      val op = ops(i)
      op.run(dstate)
      i = i + 1
    }
  }
}

class DStateForConstantFolding extends DState {
  private def die = throw new java.lang.IllegalStateException("No infoset at compile time.")

  override def currentSimple = currentNode.asInstanceOf[DISimple]
  override def currentElement = die
  override def currentArray = die
  override def currentComplex = die
  override def pstate = die
  override def currentNode = new FakeDINode
  override def vmap = die
  override def selfMove() = die
  override def fnExists() = die
}

/**
 * expression evaluation side-effects this state block.
 */
case class DState() {
  import AsIntConverters._
  /**
   * The currentValue is used when we have a value that is not
   * associated with an element of simple type. E.g., If I have
   * the expression 5 + $x, then none of those literals, nor their
   * nor variable value, nor their sum, has an element associated with it.
   */
  private var _currentValue: Any = null

  def resetValue() {
    _currentValue = null
  }

  def currentValue: Any = {
    if (_currentValue == null) currentSimple.dataValue
    else _currentValue
  }

  def setCurrentValue(v: Any) {
    _currentValue = v
    _currentNode = null
  }

  def booleanValue: Boolean = currentValue.asInstanceOf[Boolean]

  def longValue: Long = asLong(currentValue)
  def intValue: Int = longValue.toInt
  def doubleValue: Double = asDouble(currentValue)

  def integerValue: BigInt = asBigInt(currentValue)
  def decimalValue: BigDecimal = asBigDecimal(currentValue)
  def stringValue: String = currentValue.asInstanceOf[String]

  def isNilled: Boolean = currentElement.isNilled

  def arrayLength: Long = currentArray.length

  def exists: Boolean = true // we're at a node, so it must exist.

  def dateValue: DFDLCalendar = currentValue.asInstanceOf[DFDLCalendar]
  def timeValue: DFDLCalendar = currentValue.asInstanceOf[DFDLCalendar]
  def dateTimeValue: DFDLCalendar = currentValue.asInstanceOf[DFDLCalendar]

  /**
   * Array index calculations (that is [expr], what XPath
   * calls 'predicate')
   */
  def index: Int = longValue.toInt

  private var _currentNode: DINode = null

  def currentNode = _currentNode
  def setCurrentNode(n: DINode) {
    _currentNode = n
    _currentValue = null
  }

  def currentSimple = currentNode.asInstanceOf[DISimple]
  def currentElement = currentNode.asInstanceOf[DIElement]
  def currentArray = currentNode.asInstanceOf[DIArray]
  def currentComplex = currentNode.asInstanceOf[DIComplex]

  private var _vmap: VariableMap = null

  def vmap = _vmap
  def setVMap(m: VariableMap) {
    _vmap = m
  }

  private var _pstate: PState = null // for issuing processing errors

  def pstate = _pstate
  def setPState(ps: PState) {
    _pstate = ps
  }

  // These exists so we can override it in our fake DState we use when
  // checking expressions to see if they are constants. SelfMove 
  // for real is a no-op, but when we're evaluating an expression to see if 
  // it is a constant, the expression "." aka self, isn't constant. 
  // Similarly, if you call fn:exists(....) and the contents are not gong to 
  // exist at constnat fold time. But we don't want fn:exists to say the result
  // is always a constant (false) because at constant folding time, hey, nothing
  // exists... this hook lets us change behavior for constant folding to throw.
  def selfMove(): Unit = {}
  def fnExists(): Unit = {}
}

object AsIntConverters {
  /**
   * Parsers don't always insert the smallest numeric type into the infoset.
   * Sometimes we get a BigInt when an Int would have sufficed, but the
   * parsers don't always do that. This is a workaround. Really the parsers
   * should be inserting the *right thing* into the infoset.
   */
  def asInt(n: Any): Int = {
    val value = n match {
      case b: Byte => b.toInt
      case s: Short => s.toInt
      case i: Int => i
      case l: Long => l.toInt
      case bi: BigInt => bi.toInt
      case _ => Assert.invariantFailed("Unsupported conversion to Int. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    value
  }
  def asByte(n: Any): Byte = {
    val value = n match {
      case b: Byte => b
      case s: Short => s.toByte
      case i: Int => i.toByte
      case l: Long => l.toByte
      case bi: BigInt => bi.toByte
      case _ => Assert.invariantFailed("Unsupported conversion to Byte. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    value
  }
  def asShort(n: Any): Short = {
    val value = n match {
      case b: Byte => b.toShort
      case s: Short => s
      case i: Int => i.toShort
      case l: Long => l.toShort
      case bi: BigInt => bi.toShort
      case _ => Assert.invariantFailed("Unsupported conversion to Short. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    value
  }

  def asLong(n: Any): Long = {
    val value = n match {
      case b: Byte => b.toLong
      case s: Short => s.toLong
      case i: Int => i.toLong
      case l: Long => l
      case bi: BigInt => bi.toLong
      case jbi: java.math.BigInteger => jbi.longValue()
      case _ => Assert.invariantFailed("Unsupported conversion to Long. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    value
  }

  def asBigInt(n: Any): BigInt = {
    val value = n match {
      case b: Byte => BigInt(b)
      case s: Short => BigInt(s)
      case i: Int => BigInt(i)
      case l: Long => BigInt(l)
      case bi: BigInt => bi
      case _ => Assert.invariantFailed("Unsupported conversion to BigInt. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    value
  }

  def asFloat(n: Any): Float = {
    val value = n match {
      case f: Float => f
      case d: Double => d.toFloat
      case b: Byte => b.toFloat
      case s: Short => s.toFloat
      case i: Int => i.toFloat
      case l: Long => l.toFloat
      case bi: BigInt => bi.toFloat
      case bd: BigDecimal => bd.toFloat
      case _ => Assert.invariantFailed("Unsupported conversion to Float. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    value
  }

  def asDouble(n: Any): Double = {
    val value = n match {
      case f: Float => f.toDouble
      case d: Double => d
      case b: Byte => b.toDouble
      case s: Short => s.toDouble
      case i: Int => i.toDouble
      case l: Long => l.toDouble
      case bi: BigInt => bi.toDouble
      case bd: BigDecimal => bd.toDouble
      case _ => Assert.invariantFailed("Unsupported conversion to Double. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    value
  }

  def asBigDecimal(n: Any): BigDecimal = {
    val value = n match {
      case f: Float => BigDecimal(f)
      case d: Double => BigDecimal(d)
      case b: Byte => BigDecimal(b)
      case s: Short => BigDecimal(s)
      case i: Int => BigDecimal(i)
      case l: Long => BigDecimal(l)
      case bi: BigInt => BigDecimal(bi)
      case bd: BigDecimal => bd
      case _ => Assert.invariantFailed("Unsupported conversion to BigDecimal. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    value
  }

  def asBoolean(n: Any): Boolean = {
    n.asInstanceOf[Boolean]
  }
}

sealed abstract class RecipeOp
  extends Serializable {
  import AsIntConverters._

  def run(dstate: DState): Unit

  protected def subRecipes: Seq[CompiledDPath] = Nil

  protected def toXML(s: String): scala.xml.Node = toXML(new scala.xml.Text(s))

  protected def toXML(children: scala.xml.Node*): scala.xml.Node = toXML(children.toSeq)

  protected def toXML(children: scala.xml.NodeSeq): scala.xml.Node = {
    val name = Misc.getNameFromClass(this)
    scala.xml.Elem(null, name, scala.xml.Null, scala.xml.TopScope, children.isEmpty, children: _*)
  }

  /**
   * default behavior is inherited and it displays a RecipeOp assuming there are no children
   * to display. Override and make it call one of the above toXML methods if
   * there are children to display.
   */
  def toXML: scala.xml.Node = toXML(scala.xml.NodeSeq.Empty)

}

case object ToRoot extends RecipeOp {
  override def run(dstate: DState) {
    var now = dstate.currentElement
    var parent = now.diParent
    while (parent.isDefined) {
      now = parent.get
      parent = now.diParent
    }
    dstate.setCurrentNode(now)
  }
}

abstract class RecipeOpWithSubRecipes(recipes: List[CompiledDPath]) extends RecipeOp {

  override def subRecipes: List[CompiledDPath] = recipes

  def this(recipes: CompiledDPath*) = this(recipes.toList)

}

case object SelfMove extends RecipeOp {
  override def run(dstate: DState) {
    // do this entirely so it will fail at constant compile time
    // also serves as a sort of assertion check.
    dstate.selfMove()
  }
}

case object UpMove extends RecipeOp {
  override def run(dstate: DState) {
    val now = dstate.currentElement
    val n = now.diParent.getOrElse(
      Assert.invariantFailed(
        "UpMove past root. Should never happen since an expression like that won't typecheck statically."))
    dstate.setCurrentNode(n)
  }
}

/**
 * Down to a non-array element. Can be optional or scalar.
 */
case class DownElement(info: DPathElementCompileInfo) extends RecipeOp {

  override def run(dstate: DState) {
    val now = dstate.currentComplex
    // TODO PE ? if doesn't exist should be a processing error.
    // It will throw and so will be a PE, but may be poor diagnostic.
    dstate.setCurrentNode(now.getChild(info.slotIndexInParent, info.name, info.namedQName.namespace).asInstanceOf[DIElement])
  }

  override def toXML = {
    toXML(info.name)
  }

}

/**
 * Move down to an occurrence of an array element.
 */
case class DownArrayOccurrence(info: DPathElementCompileInfo, indexRecipe: CompiledDPath)
  extends RecipeOpWithSubRecipes(indexRecipe) {

  override def run(dstate: DState) {
    val savedCurrentElement = dstate.currentComplex
    indexRecipe.run(dstate)
    val index = dstate.index
    Assert.invariant(dstate.index > 0) // TODO PE?
    val arr = savedCurrentElement.getChildArray(info.slotIndexInParent)
    Assert.invariant(arr.isDefined) // TODO PE?
    val occurrence = arr.get.getOccurrence(index) // will throw on out of bounds
    dstate.setCurrentNode(occurrence.asInstanceOf[DIElement])
  }

  override def toXML = {
    toXML(new scala.xml.Text(info.name) ++ indexRecipe.toXML)
  }

  //  @throws(classOf[java.io.IOException])
  //  final private def writeObject(out: java.io.ObjectOutputStream): Unit = {
  //    preSerialization
  //    out.defaultWriteObject()
  //  }
}

/*
 * down to an array object containing all occurrences
 */
case class DownArray(info: DPathElementCompileInfo) extends RecipeOp {

  override def run(dstate: DState) {
    val now = dstate.currentComplex
    val arr = now.getChildArray(info.slotIndexInParent)
    Assert.invariant(arr.isDefined) // TODO PE?
    dstate.setCurrentNode(arr.get.asInstanceOf[DIArray])
  }

  override def toXML = {
    toXML(info.name)
  }

}

case object FNCount extends RecipeOp {
  override def run(dstate: DState) {
    dstate.setCurrentValue(dstate.arrayLength)
  }
}

case object DFDLOccursIndex extends RecipeOp {
  override def run(dstate: DState) {
    dstate.setCurrentValue(dstate.pstate.mpstate.arrayPos)
  }
}

case class DFDLCheckConstraints(recipe: CompiledDPath) extends RecipeOpWithSubRecipes(recipe) {
  override def run(dstate: DState) {
    recipe.run(dstate)
    if (dstate.currentElement.valid.isDefined) {
      dstate.setCurrentValue(dstate.currentElement.valid.get)
    } else {
      val res = DFDLCheckConstraintsFunction.executeCheck(dstate.currentSimple) match {
        case Right(boolVal) => true
        case Left(msg) => false
      }
      dstate.currentElement.setValid(res)
      dstate.setCurrentValue(res)
    }
  }
}

case class DFDLDecodeDFDLEntities(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = {
    val dfdlString = EntityReplacer { _.replaceAll(str.asInstanceOf[String], None) }
    dfdlString
  }
}

case class DFDLEncodeDFDLEntities(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = constructLiteral(str.asInstanceOf[String])

  def constructLiteral(s: String) = {
    val sb = new StringBuilder
    s.foreach(c => {
      c match {
        case '%' => sb.append("%%") // \u0025
        case '\u0000' | 0xE000 => sb.append("%NUL;")
        case '\u0001' | 0xE001 => sb.append("%SOH;")
        case '\u0002' | 0xE002 => sb.append("%STX;")
        case '\u0003' | 0xE003 => sb.append("%ETX;")
        case '\u0004' | 0xE004 => sb.append("%EOT;")
        case '\u0005' | 0xE005 => sb.append("%ENQ;")
        case '\u0006' | 0xE006 => sb.append("%ACK;")
        case '\u0007' | 0xE007 => sb.append("%BEL;")
        case '\u0008' | 0xE008 => sb.append("%BS;")
        case '\u0009' => sb.append("%HT;") // OK, not remapped
        case '\u000A' => sb.append("%LF;") // OK, not remapped
        case '\u000B' | 0xE00B => sb.append("%VT;")
        case '\u000C' | 0xE00C => sb.append("%FF;")
        case '\u000D' => sb.append("%CR;") // OK, not remapped
        case '\u000E' | 0xE00E => sb.append("%SO;")
        case '\u000F' | 0xE00F => sb.append("%SI;")
        case '\u0010' | 0xE010 => sb.append("%DLE;")
        case '\u0011' | 0xE011 => sb.append("%DC1;")
        case '\u0012' | 0xE012 => sb.append("%DC2;")
        case '\u0013' | 0xE013 => sb.append("%DC3;")
        case '\u0014' | 0xE014 => sb.append("%DC4;")
        case '\u0015' | 0xE015 => sb.append("%NAK;")
        case '\u0016' | 0xE016 => sb.append("%SYN;")
        case '\u0017' | 0xE017 => sb.append("%ETB;")
        case '\u0018' | 0xE018 => sb.append("%CAN;")
        case '\u0019' | 0xE019 => sb.append("%EM;") // and above remapped to c + 0xE000
        case '\u001A' => sb.append("%SUB;")
        case '\u001B' => sb.append("%ESC;")
        case '\u001C' => sb.append("%FS;")
        case '\u001D' => sb.append("%GS;")
        case '\u001E' => sb.append("%RS;")
        case '\u001F' => sb.append("%US;")
        case '\u0020' => sb.append("%SP;")
        case '\u007F' => sb.append("%DEL;")
        case '\u00A0' => sb.append("%NBSP;")
        case '\u0085' => sb.append("%NEL;")
        case '\u2028' => sb.append("%LS;")
        case _ => sb.append(c)
      }
    })
    sb.toString()
  }
}

case class DFDLContainsDFDLEntities(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) =
    EntityReplacer { _.hasDfdlEntity(str.asInstanceOf[String]) }
}

case class DFDLTestBit(dataRecipe: CompiledDPath, bitPos1bRecipe: CompiledDPath)
  extends RecipeOpWithSubRecipes(dataRecipe, bitPos1bRecipe) {

  override def run(dstate: DState) {
    val saved = dstate.currentNode
    dataRecipe.run(dstate)
    val dataVal = dstate.intValue
    dstate.setCurrentNode(saved)
    bitPos1bRecipe.run(dstate)
    val bitPos1b = dstate.intValue
    checkRange(bitPos1b)
    val res = testBit(dataVal, bitPos1b)
    dstate.setCurrentValue(res)
  }

  private def checkRange(i: Int) = {
    if (i > 8 || i < 1) {
      throw new SchemaDefinitionError(None, None,
        "dfdl:testBit $bitPos must be between 1 and 8 (inclusive). Was %s.", i)
    }
  }

  private def testBit(data: Int, bitPos1b: Int): Boolean = {
    // Assume 8-bit
    val shifted = data >>> (bitPos1b - 1)
    val maskedVal = shifted & 1
    if (maskedVal == 1) true
    else false
  }
}

object withArray8 extends OnStack(new Array[Int](8))

case class DFDLSetBits(bitRecipes: List[CompiledDPath]) extends RecipeOpWithSubRecipes(bitRecipes) {

  override def run(dstate: DState) {
    Assert.invariant(bitRecipes.length == 8)
    val saved = dstate.currentNode
    withArray8 { ar =>
      {
        var i = 0
        var bitR = bitRecipes
        while (i < 8) {
          val br = bitR.head
          dstate.setCurrentNode(saved)
          br.run(dstate)
          val currentVal = dstate.intValue
          ar(i) = currentVal
          i += 1
          bitR = bitR.tail
        }
        // at this point we have ar with 8 values in it.
        val byteVal = setBits(ar)
        dstate.setCurrentValue(byteVal)
      }
    }
  }

  private def processValue(i: Int): Boolean = {
    if (i < 0 || i > 1) throw new IllegalArgumentException("dfdl:setBits arguments must each be 0 or 1, but value was: %s.".format(i))
    if (i == 0) false
    else true
  }

  private def setBits(args: Array[Int]): Int = {
    val bp0 = processValue(args(0))
    val bp1 = processValue(args(1))
    val bp2 = processValue(args(2))
    val bp3 = processValue(args(3))
    val bp4 = processValue(args(4))
    val bp5 = processValue(args(5))
    val bp6 = processValue(args(6))
    val bp7 = processValue(args(7))
    var uByte: Int = 0
    if (bp0) uByte += 1
    if (bp1) uByte += 2
    if (bp2) uByte += 4
    if (bp3) uByte += 8
    if (bp4) uByte += 16
    if (bp5) uByte += 32
    if (bp6) uByte += 64
    if (bp7) uByte += 128
    uByte
  }
}

case class VRef(qn: RefQName, context: ThrowsSDE)
  extends RecipeOp {

  val expName = qn.toExpandedName

  override def run(dstate: DState) {
    Assert.invariant(dstate.vmap != null)
    val (res, newVMap) = dstate.vmap.readVariable(expName, context)
    dstate.setVMap(newVMap)
    dstate.setCurrentValue(res)
  }

  override def toXML = toXML("$" + qn.toPrettyString)

}

case class Literal(v: Any) extends RecipeOp {
  override def run(dstate: DState) {
    dstate.setCurrentValue(v)
  }
  override def toXML = toXML(v.toString)
}

case class IF(predRecipe: CompiledDPath, thenPartRecipe: CompiledDPath, elsePartRecipe: CompiledDPath)
  extends RecipeOpWithSubRecipes(predRecipe, thenPartRecipe, elsePartRecipe) {

  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    predRecipe.run(dstate)
    val predValue = dstate.currentValue.asInstanceOf[Boolean]
    dstate.setCurrentNode(savedNode)
    if (predValue) {
      thenPartRecipe.run(dstate)
    } else {
      elsePartRecipe.run(dstate)
    }
    // should have a value now. IF-Then-Else is always
    // evaluated for a value.
    Assert.invariant(dstate.currentValue != null)
  }

  override def toXML =
    <if>
      <pred>{ predRecipe.toXML }</pred>
      <then>{ thenPartRecipe.toXML }</then>
      <else>{ elsePartRecipe.toXML }</else>
    </if>
}

trait BinaryOpMixin { self: RecipeOp =>
  def op: String
  def left: CompiledDPath
  def right: CompiledDPath
  override def subRecipes: Seq[CompiledDPath] = Seq(left, right)

  override def toXML: scala.xml.Node = toXML(new scala.xml.Text(op), left.toXML, right.toXML)
}

case class NumberCompareOperator(cop: NumberCompareOp, left: CompiledDPath, right: CompiledDPath)
  extends RecipeOp with BinaryOpMixin {

  override def op = Misc.getNameFromClass(cop)

  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    left.run(dstate)
    val leftValue = dstate.currentValue
    dstate.setCurrentNode(savedNode)
    right.run(dstate)
    val rightValue = dstate.currentValue
    val result = cop.operate(leftValue, rightValue)
    dstate.setCurrentValue(result)
  }
}

trait NumberCompareOp {
  /**
   * It is such a pain that there is no scala.math.Number base class above
   * all the numeric types.
   */
  def operate(v1: Any, v2: Any): Boolean
}

abstract class CompareOp
  extends RecipeOp with BinaryOpMixin {

  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    left.run(dstate)
    val leftValue = dstate.currentValue
    // Now reset back to the original node to evaluate the right
    dstate.setCurrentNode(savedNode)
    right.run(dstate)
    val rightValue = dstate.currentValue
    val result = compare(op, leftValue, rightValue)
    dstate.setCurrentValue(result)
  }

  def compare(op: String, v1: Any, v2: Any): Boolean
}

case class EqualityCompareOp(op: String, left: CompiledDPath, right: CompiledDPath)
  extends CompareOp {

  def compare(op: String, v1: Any, v2: Any): Boolean = {
    (op, v1, v2) match {
      case ("=", a, b) => a == b
      case ("eq", a, b) => a == b
      case ("!=", a, b) => a != b
      case ("ne", a, b) => a != b
      case _ => Assert.notYetImplemented("operator " + op +
        " on types " + Misc.getNameFromClass(v1) + ", " +
        Misc.getNameFromClass(v2))
    }
  }
}

case class BooleanOp(op: String, left: CompiledDPath, right: CompiledDPath)
  extends RecipeOp with BinaryOpMixin {
  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    left.run(dstate)
    val leftValue = dstate.currentValue.asInstanceOf[Boolean] // convertToBoolean(dstate.currentValue, dstate.pstate)
    dstate.setCurrentNode(savedNode)
    right.run(dstate)
    val rightValue = dstate.currentValue.asInstanceOf[Boolean] // convertToBoolean(dstate.currentValue, dstate.pstate)
    val result = operateBoolean(op, leftValue, rightValue)
    dstate.setCurrentValue(result)
  }

  def operateBoolean(op: String, left: Boolean, right: Boolean): Boolean = {
    (op, left, right) match {
      case ("and", v1, v2) => v1 && v2
      case ("or", v1, v2) => v1 || v2
      case _ => Assert.usageError("Not a boolean op")
    }
  }
}
case class NegateOp(recipe: CompiledDPath) extends RecipeOpWithSubRecipes(recipe) {
  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    recipe.run(dstate)
    val value = dstate.currentValue match {
      case i: Int => i * -1
      case l: Long => l * (-1L)
      case d: Double => d * 1.0
      case bi: BigInt => bi * -1
      case bd: BigDecimal => bd * -1
      case _ => Assert.invariantFailed("not a number: " + dstate.currentValue.toString)
    }
    dstate.setCurrentValue(value)
  }

  override def toXML: scala.xml.Node = <Negate>{ recipe.toXML }</Negate>
}

case class FNDateTime(recipes: List[CompiledDPath]) extends FNTwoArgs(recipes) {
  val name = "FNDateTime"

  private def calendarToDFDLDateTime(calendar: Calendar, formatString: String, dstate: DState, fncName: String, toType: String): DFDLCalendar = {
    try {
      val cal = new DFDLDateTime(calendar)
      return cal
    } catch {
      case ex: java.lang.IllegalArgumentException =>
        dstate.pstate.SDE("Conversion Error: %s failed to convert \"%s\" to %s. Due to %s", fncName, calendar.toString, toType, ex.getMessage())
    }
  }
  override def computeValue(arg1: Any, arg2: Any, dstate: DState) = {

    val dateCal = arg1.asInstanceOf[DFDLCalendar].getCalendar
    val timeCal = arg2.asInstanceOf[DFDLCalendar].getCalendar

    val year = dateCal.get(Calendar.YEAR)
    val day = dateCal.get(Calendar.DAY_OF_MONTH)
    val month = dateCal.get(Calendar.MONTH)
    val dateTZ = dateCal.getTimeZone()

    val timeTZ = timeCal.getTimeZone()

    val newCal: Calendar = timeCal.clone().asInstanceOf[Calendar]
    newCal.set(Calendar.YEAR, year)
    newCal.set(Calendar.DAY_OF_MONTH, day)
    newCal.set(Calendar.MONTH, month)

    /**
     * http://www.w3.org/TR/xpath-functions/#func-dateTime
     *
     * The timezone of the result is computed as follows:
     *
     * If neither argument has a timezone, the result has no timezone.
     * If exactly one of the arguments has a timezone, or if both arguments
     * 	have the same timezone, the result has this timezone.
     * If the two arguments have different timezones, an error
     * 	is raised:[err:FORG0008]
     */

    (dateTZ, timeTZ) match {
      case (null, null) => newCal.setTimeZone(null)
      case (tz, null) => newCal.setTimeZone(tz)
      case (null, tz) => newCal.setTimeZone(tz)
      case (tz0, tz1) if tz0 != tz1 => dstate.pstate.SDE("The two arguments to fn:dateTime have inconsistent timezones")
      case (tz0, tz1) => newCal.setTimeZone(tz0)
    }

    val finalCal = calendarToDFDLDateTime(newCal, "uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxxxx", dstate, name, "DateTime")
    finalCal
  }
}

case class FNRoundHalfToEven(recipeNum: CompiledDPath, recipePrecision: CompiledDPath)
  extends RecipeOpWithSubRecipes(recipeNum, recipePrecision) {

  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    recipeNum.run(dstate)
    val unrounded = dstate.currentValue
    dstate.setCurrentNode(savedNode)
    recipePrecision.run(dstate)
    val precision = dstate.intValue
    val bd = unrounded match {
      case s: String => BigDecimal(s) // TODO: Remove eventually. Holdover from JDOM where everything is a string.
      case l: Long => BigDecimal.valueOf(l)
      case f: Float => BigDecimal.valueOf(f)
      case d: Double => BigDecimal.valueOf(d)
      case bd: BigDecimal => bd
      case _ => Assert.invariantFailed("not a number")
    }
    val value = {
      val rounded = bd.setScale(precision, BigDecimal.RoundingMode.HALF_EVEN)
      rounded
    }
    dstate.setCurrentValue(value)
  }
}

case class FNNot(recipe: CompiledDPath, argType: NodeInfo.Kind = null) extends FNOneArg(recipe, NodeInfo.Boolean) {
  override def computeValue(value: Any, dstate: DState) = !(value.asInstanceOf[Boolean])
}

case class FNEndsWith(recipes: List[CompiledDPath]) extends FNTwoArgs(recipes) {
  override def computeValue(arg1: Any, arg2: Any, dstate: DState) = {
    val arg1s = arg1.asInstanceOf[String]
    val arg2s = arg2.asInstanceOf[String]
    val res = arg1s.endsWith(arg2s)
    res
  }
}

case class FNNilled(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, NodeInfo.Nillable) {
  override def computeValue(value: Any, dstate: DState) = value.asInstanceOf[DIElement].isNilled
}

case class FNExists(recipe: CompiledDPath, argType: NodeInfo.Kind) extends RecipeOpWithSubRecipes(recipe) {
  override def run(dstate: DState) {
    dstate.fnExists() // hook so we can insist this is non-constant at compile time.
    val exists =
      try {
        recipe.run(dstate)
        true
      } catch {
        // catch exceptions indicating a node (or value) doesn't exist.
        //
        // if you reach into an array location that doesn't exist
        case e: java.lang.IndexOutOfBoundsException => false
        // if you reach into a child element slot that isn't filled
        case e: InfosetNoSuchChildElementException => false
        // if something else goes wrong while evaluating the
        // expression (fn:exist can be called on expressions that are
        // not necessarily nodes.They can be simple value expressions)
        case e: java.lang.IllegalStateException => false
        case e: java.lang.NumberFormatException => false
        case e: java.lang.IllegalArgumentException => false
        case e: java.lang.ArithmeticException => false
        case e: ProcessingError => false
      }
    dstate.setCurrentValue(exists)
  }

  override def toXML = toXML(recipe.toXML)

}

case class FNCeiling(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(value: Any, dstate: DState) = argType match {

    case NodeInfo.Decimal => {
      val bd = asBigDecimal(value).setScale(0, RoundingMode.CEILING)
      bd.round(bd.mc)
    }
    case NodeInfo.Float => asFloat(value).ceil
    case NodeInfo.Double => asDouble(value).ceil
    case _: NodeInfo.Numeric.Kind => value
    case _ => Assert.invariantFailed(String.format("Type %s is not a valid type for function ceiling.", argType))
  }
}

case class FNFloor(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(value: Any, dstate: DState) = argType match {

    case NodeInfo.Decimal => {
      val bd = asBigDecimal(value).setScale(0, RoundingMode.FLOOR)
      bd.round(bd.mc)
    }
    case NodeInfo.Float => asFloat(value).floor
    case NodeInfo.Double => asDouble(value).floor
    case _: NodeInfo.Numeric.Kind => value
    case _ => Assert.invariantFailed(String.format("Type %s is not a valid type for function floor.", argType))
  }
}

case class FNRound(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(value: Any, dstate: DState) = argType match {

    case NodeInfo.Decimal => {
      val bd = asBigDecimal(value)
      bd.round(bd.mc)
    }
    case NodeInfo.Float => asFloat(value).round
    case NodeInfo.Double => asDouble(value).round
    case _: NodeInfo.Numeric.Kind => value
    case _ => Assert.invariantFailed(String.format("Type %s is not a valid type for function round.", argType))
  }
}

case class FNAbs(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(value: Any, dstate: DState) = argType match {
    case _: NodeInfo.UnsignedNumeric.Kind => value
    case NodeInfo.Decimal => asBigDecimal(value).abs
    case NodeInfo.Float => asFloat(value).abs
    case NodeInfo.Double => asDouble(value).abs
    case NodeInfo.Long => asLong(value).abs
    case NodeInfo.Int => asInt(value).abs
    case NodeInfo.Integer => asBigInt(value).abs
    case NodeInfo.Short => asShort(value).abs
    case NodeInfo.Byte => asByte(value).abs
    case _ => Assert.invariantFailed(String.format("Type %s is not a valid type for function abs.", argType))
  }
}

case class FNStringLength(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = str.asInstanceOf[String].length.toLong
}

case class FNLowerCase(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = str.asInstanceOf[String].toLowerCase
}

case class FNUpperCase(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = str.asInstanceOf[String].toUpperCase
}

case class FNConcat(recipes: List[CompiledDPath]) extends FNArgsList(recipes) {
  override def computeValue(values: List[Any], dstate: DState) = values.mkString
}

// No such function in DFDL v1.0
// But leave this here because we probably will want to add it as a 
// daffodil extension function and then eventually hope it gets into DFDL1.1
//case class FNStringJoin(recipes: List[CompiledDPath]) extends FNTwoArgs(recipes) {
//  override def computeValue(arg1: Any, arg2: Any, dstate: DState) = {
//    val values = arg1.asInstanceOf[List[String]]
//    val sep = arg2.asInstanceOf[String]
//    values.mkString(sep)
//  }
//}

case class FNSubstring2(recipes: List[CompiledDPath]) extends FNTwoArgs(recipes) {
  override def computeValue(arg1: Any, arg2: Any, dstate: DState) = {
    val sourceString = arg1.asInstanceOf[String]
    val startPos = asDouble(arg2)
    sourceString.substring(startPos.toInt - 1)
  }
}

case class FNSubstring3(recipes: List[CompiledDPath]) extends FNThreeArgs(recipes) {
  override def computeValue(arg1: Any, arg2: Any, arg3: Any, dstate: DState) = {
    val sourceString = arg1.asInstanceOf[String]
    val startPos = asDouble(arg2).toInt - 1 //adjust to zero-based
    val length = asDouble(arg3).toInt
    sourceString.substring(startPos, length)
  }
}

abstract class FNOneArg(recipe: CompiledDPath, argType: NodeInfo.Kind) extends RecipeOpWithSubRecipes(recipe) {
  override def run(dstate: DState) {
    recipe.run(dstate)
    val arg = dstate.currentValue
    dstate.setCurrentValue(computeValue(arg, dstate))
  }

  override def toXML = toXML(recipe.toXML)

  def computeValue(str: Any, dstate: DState): Any
}

abstract class FNTwoArgs(recipes: List[CompiledDPath]) extends RecipeOpWithSubRecipes(recipes) {
  override def run(dstate: DState) {

    val List(recipe1, recipe2) = recipes

    val savedNode = dstate.currentNode
    dstate.resetValue()
    recipe1.run(dstate)
    val arg1 = dstate.currentValue

    dstate.setCurrentNode(savedNode)
    recipe2.run(dstate)
    val arg2 = dstate.currentValue

    dstate.setCurrentValue(computeValue(arg1, arg2, dstate))
  }

  def computeValue(arg1: Any, arg2: Any, dstate: DState): Any

  override def toXML = toXML(recipes.map { _.toXML })
}

abstract class FNThreeArgs(recipes: List[CompiledDPath]) extends RecipeOpWithSubRecipes(recipes) {
  override def run(dstate: DState) {

    val List(recipe1, recipe2, recipe3) = recipes

    val savedNode = dstate.currentNode
    recipe1.run(dstate)
    val arg1 = dstate.currentValue

    dstate.setCurrentNode(savedNode)
    recipe2.run(dstate)
    val arg2 = dstate.currentValue

    dstate.setCurrentNode(savedNode)
    recipe3.run(dstate)
    val arg3 = dstate.currentValue
    dstate.setCurrentValue(computeValue(arg1, arg2, arg3, dstate))
  }

  def computeValue(arg1: Any, arg2: Any, arg3: Any, dstate: DState): Any

  override def toXML = toXML(recipes.map { _.toXML })
}

trait FNFromDateTimeKind {
  def fieldName: String
  def field: Int
}

abstract class FNFromDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType)
  with FNFromDateTimeKind {
  override def computeValue(a: Any, dstate: DState) = {
    a match {
      case dt: DFDLDateTime => dt.getField(field)
      case _ => throw new NumberFormatException("fn:" + fieldName + "-from-dateTime only accepts xs:dateTime.")
    }
  }
}

abstract class FNFromDate(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType)
  with FNFromDateTimeKind {
  override def computeValue(a: Any, dstate: DState) = {
    a match {
      case d: DFDLDate => d.getField(field)
      case _ => throw new NumberFormatException("fn:" + fieldName + "-from-date only accepts xs:date.")
    }
  }
}

abstract class FNFromTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType)
  with FNFromDateTimeKind {
  override def computeValue(a: Any, dstate: DState) = {
    a match {
      case t: DFDLTime => t.getField(field)
      case _ => throw new NumberFormatException("fn:" + fieldName + "-from-time only accepts xs:time.")
    }
  }
}

case class FNYearFromDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDateTime(recipe, argType) {
  val fieldName = "year"
  val field = Calendar.YEAR
}
case class FNMonthFromDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDateTime(recipe, argType) {
  val fieldName = "month"
  val field = Calendar.MONTH
  override def computeValue(a: Any, dstate: DState) = super.computeValue(a, dstate).asInstanceOf[Int] + 1 // JAN 0
}
case class FNDayFromDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDateTime(recipe, argType) {
  val fieldName = "day"
  val field = Calendar.DAY_OF_MONTH
}
case class FNHoursFromDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDateTime(recipe, argType) {
  val fieldName = "hours"
  val field = Calendar.HOUR_OF_DAY
}
case class FNMinutesFromDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDateTime(recipe, argType) {
  val fieldName = "minutes"
  val field = Calendar.MINUTE
}
case class FNSecondsFromDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDateTime(recipe, argType) {
  val fieldName = "seconds"
  val field = Calendar.SECOND
}

case class FNYearFromDate(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDate(recipe, argType) {
  val fieldName = "year"
  val field = Calendar.YEAR
}
case class FNMonthFromDate(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDate(recipe, argType) {
  val fieldName = "month"
  val field = Calendar.MONTH
  override def computeValue(a: Any, dstate: DState) = super.computeValue(a, dstate).asInstanceOf[Int] + 1 // JAN 0
}
case class FNDayFromDate(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDate(recipe, argType) {
  val fieldName = "day"
  val field = Calendar.DAY_OF_MONTH
}
case class FNHoursFromTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromTime(recipe, argType) {
  val fieldName = "hours"
  val field = Calendar.HOUR_OF_DAY
}
case class FNMinutesFromTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromTime(recipe, argType) {
  val fieldName = "minutes"
  val field = Calendar.MINUTE
}
case class FNSecondsFromTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromTime(recipe, argType) {
  val fieldName = "seconds"
  val field = Calendar.SECOND
}

abstract class FNArgsList(recipes: List[CompiledDPath]) extends RecipeOpWithSubRecipes(recipes) {
  override def run(dstate: DState) {

    val savedNode = dstate.currentNode

    // FIXME: rewrite to use an OnStack ListBuffer, and
    // a while loop with index vs. the foreach. 
    val args: List[Any] = {
      val list = new ListBuffer[Any]

      recipes.foreach { recipe =>
        recipe.run(dstate)
        list += dstate.currentValue
        dstate.setCurrentNode(savedNode)
      }
      list.toList
    }

    dstate.setCurrentValue(computeValue(args, dstate))
  }

  def computeValue(args: List[Any], dstate: DState): Any
}

case class XSInt(recipe: CompiledDPath) extends RecipeOpWithSubRecipes(recipe) {
  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    recipe.run(dstate)
    val basicValue = dstate.currentValue
    val value = asInt(basicValue)
    dstate.setCurrentValue(value)
  }
}

case class XSString(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(value: Any, dstate: DState) = {
    val res: Any = value match {
      case hb: Array[Byte] => HexBinaryToString.computeValue(hb, dstate)
      case _ => value.toString
    }
    res
  }
}

case class XSDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  val name = "XSDateTime"

  override def computeValue(a: Any, dstate: DState): Any = {
    val result = a match {
      case _: DFDLTime => throw new NumberFormatException("Casting from xs:time to xs:dateTime can never succeed.")
      case _ => ToDateTime.computeValue(a, dstate)
    }
    result
  }
}

case class XSDate(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  val name = "XSDate"

  override def computeValue(a: Any, dstate: DState): Any = {
    val result = a match {
      case _: DFDLTime => throw new NumberFormatException("Casting from xs:time to xs:date can never succeed.")
      case _ => ToDate.computeValue(a, dstate)
    }
    result
  }
}

case class XSTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  val name = "XSTime"

  override def computeValue(a: Any, dstate: DState): Any = {
    val result = a match {
      case _: DFDLDate => throw new NumberFormatException("Casting from xs:date to xs:time can never succeed")
      case _ => ToTime.computeValue(a, dstate)
    }
    result
  }
}

trait HexBinaryKind {

  private val conversionErrMsg: String = "%s could not be represented as a long."

  /**
   * http://travisdazell.blogspot.com/2012/11/converting-hex-string-to-byte-array-in.html
   */
  protected def hexStringToByteArray(str: String): Array[Byte] = {
    val len = str.length

    if ((len % 2) != 0)
      throw new NumberFormatException("Failed to evaluate expression: A hexBinary value must contain an even number of characters.")

    val arr = new Array[Byte](len / 2)
    var i = 0
    while (i < len) {
      val upper = Character.digit(str.charAt(i), 16)
      val lower = Character.digit(str.charAt(i + 1), 16)

      if (upper == -1)
        throw new NumberFormatException("Failed to evaluate expression: Invalid hexadecimal digit '%c' at index %d of '%s'".format(str.charAt(i), i, str))
      if (lower == -1)
        throw new NumberFormatException("Failed to evaluate expression: Invalid hexadecimal digit '%c' at index %d of '%s'".format(str.charAt(i + 1), i + 1, str))

      val byte = (upper << 4) + (lower)
      arr(i / 2) = byte.asInstanceOf[Byte]
      i += 2
    }
    return arr
  }

  protected def reduce(numeric: Any): Array[Byte] = {
    val res: Array[Byte] = numeric match {
      case b: Byte => HexBinaryConversions.toByteArray(b)
      case s: Short if (s <= Byte.MaxValue && s >= Byte.MinValue) => reduce(s.toByte)
      case s: Short => HexBinaryConversions.toByteArray(s)
      case i: Int if (i <= Short.MaxValue && i >= Short.MinValue) => reduce(i.toShort)
      case i: Int => HexBinaryConversions.toByteArray(i)
      case l: Long if (l <= Int.MaxValue && l >= Int.MinValue) => reduce(l.toInt)
      case l: Long => HexBinaryConversions.toByteArray(l)
      case bi: BigInt if (bi.isValidLong) => reduce(bi.toLong)
      case bd: BigDecimal if (bd.isValidLong) => reduce(bd.toLong)
      case str: String => reduce(BigInt(str))
      case _ => throw new NumberFormatException("%s could not fit into a long".format(numeric.toString))
    }
    res
  }

  /**
   * http://javarevisited.blogspot.com/2013/03/convert-and-print-byte-array-to-hex-string-java-example-tutorial.html
   */
  protected def bytesToHexString(bytes: Array[Byte]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
      sb.append("%02X".format(b & 0xFF))
    }
    return sb.toString
  }
}

case class XSHexBinary(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) with HexBinaryKind {
  val name = "XSHexBinary"

  override def computeValue(a: Any, dstate: DState): Any = {
    // Check for:
    // 1. Even number of characters
    // 2. Valid hex (0-9 A-F)
    val array = a match {
      case s: String => hexStringToByteArray(s)
      case hb: Array[Byte] => hb
      case x => throw new NumberFormatException("%s cannot be cast to dfdl:hexBinary\ndfdl:hexBinary received an unrecognized type! Must be String or HexBinary.".format(x.toString))
    }
    array
  }
}

case class NumericOperator(nop: NumericOp, left: CompiledDPath, right: CompiledDPath)
  extends RecipeOp with BinaryOpMixin {

  override def op = Misc.getNameFromClass(nop)

  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    left.run(dstate)
    val leftValue = dstate.currentValue
    dstate.setCurrentNode(savedNode)
    right.run(dstate)
    val rightValue = dstate.currentValue
    val result = nop.operate(leftValue, rightValue)
    dstate.setCurrentValue(result)
  }
}

trait NumericOp {
  /**
   * It is such a pain that there is no scala.math.Number base class above
   * all the numeric types.
   */
  def operate(v1: Any, v2: Any): Any
}

case class DAFTrace(recipe: CompiledDPath, msg: String)
  extends FNOneArg(recipe, NodeInfo.AnyType) {

  override def computeValue(str: Any, dstate: DState) = {
    System.err.println("trace " + msg + ":" + str.toString)
    str
  }
}

abstract class Converter extends RecipeOp {

  def typeNames = {
    // This is a total hack. Grab the type names of this converter
    // by spliting the class name at the "To" in the middle.
    val names = Misc.getNameFromClass(this).split("To").toList
    Assert.invariant(names.length == 2)
    val List(fromTypeName, toTypeName) = names
    (fromTypeName, toTypeName)
  }

  override def run(dstate: DState) {
    val arg = dstate.currentValue
    val res =
      try {
        computeValue(arg, dstate)
      } catch {
        case e: NumberFormatException => {
          val (fromTypeName, toTypeName) = typeNames
          val msg =
            if (e.getMessage() != null && e.getMessage() != "") e.getMessage()
            else "No other details are available."
          val err = new NumberFormatException("Cannot convert '%s' from %s type to %s (%s).".format(arg.toString, fromTypeName, toTypeName, msg))
          throw err
        }
      }
    dstate.setCurrentValue(res)
  }

  def computeValue(str: Any, dstate: DState): Any
}

case object AnyAtomicToString extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    Console.out.println(a.getClass)
    a match {
      case c: DFDLCalendar => c.toString
      case _ => a.asInstanceOf[String]
    }
  }
}

case object BooleanToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = if (asBoolean(a) == true) 1L else 0L
}

case object BooleanToString extends Converter {
  override def computeValue(a: Any, dstate: DState) = if (asBoolean(a) == true) "true" else "" // empty string is false in XPath
}

case object DateTimeToDate extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    a match {
      case dt: DFDLDateTime => dt.toDate
      case _ => throw new NumberFormatException("xs:dateTime expected but an invalid type was received.")
    }
  }
}
case object DateTimeToTime extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    a match {
      case dt: DFDLDateTime => dt.toTime
      case _ => throw new NumberFormatException("xs:dateTime expected but an invalid type was received.")
    }
  }
}
case object DateToDateTime extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    a match {
      case d: DFDLDate => d.toDateTime
      case _ => throw new NumberFormatException("xs:date expected but an invalid type was received.")
    }
  }
}
case object DecimalToInteger extends Converter {
  override def computeValue(a: Any, dstate: DState) = asBigDecimal(a).toBigInt()
}
case object DecimalToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asBigDecimal(a)
    if (res < Long.MinValue || res > Long.MaxValue) throw new NumberFormatException("Value %s out of range for Long type.".format(res))
    res.toLong
  }
}
case object DecimalToDouble extends Converter {
  override def computeValue(a: Any, dstate: DState) = asBigDecimal(a).toDouble
}
case object DecimalToNonNegativeInteger extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asBigDecimal(a)
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to a non-negative integer.".format(res))
    res.toBigInt()
  }
}
case object DecimalToUnsignedLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asBigDecimal(a).toBigInt
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to a non-negative integer.".format(res))

    if (res > NodeInfo.UnsignedLong.Max) throw new NumberFormatException("Value %s out of range for UnsignedLong type.".format(res))
    else res
  }
}
case object DoubleToDecimal extends Converter {
  override def computeValue(a: Any, dstate: DState) = BigDecimal(asDouble(a))
}
case object DoubleToFloat extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val d = asDouble(a)
    val res = d.toFloat
    res
  }
}
case object DoubleToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asBigDecimal(asDouble(a))
    if (res < Long.MinValue || res > Long.MaxValue) throw new NumberFormatException("Value %s out of range for Long type.".format(res))
    res.toLong
  }
}
case object DoubleToUnsignedLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = BigDecimal(asDouble(a)).toBigInt
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to an unsigned long.".format(res))
    if (res > NodeInfo.UnsignedLong.Max) throw new NumberFormatException("Value %s out of range for UnsignedLong type.".format(res))
    else res
  }
}
case object FloatToDouble extends Converter {
  override def computeValue(a: Any, dstate: DState) = asFloat(a).toDouble
}
case object IntegerToDecimal extends Converter {
  override def computeValue(a: Any, dstate: DState) = BigDecimal(asBigInt(a))
}
case object IntegerToUnsignedLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asBigInt(a)
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to an unsigned long.".format(res))
    if (res > NodeInfo.UnsignedLong.Max) throw new NumberFormatException("Value %s out of range for UnsignedLong type.".format(res))
    else res
  }
}
case object LongToBoolean extends Converter {
  override def computeValue(a: Any, dstate: DState) = if (asLong(a) == 0) false else true
}
case object LongToByte extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val l = asLong(a)
    if (l > Byte.MaxValue || l < Byte.MinValue) throw new NumberFormatException("Value %s out of range for Byte type.".format(l))
    l.toByte
  }
}
case object LongToDecimal extends Converter {
  override def computeValue(a: Any, dstate: DState) = BigDecimal(asLong(a))
}
case object LongToDouble extends Converter {
  override def computeValue(a: Any, dstate: DState) = asLong(a).toDouble
}
case object LongToFloat extends Converter {
  override def computeValue(a: Any, dstate: DState) = asLong(a).toFloat
}
case object LongToInt extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val l = asLong(a)
    if (l > Int.MaxValue || l < Int.MinValue) throw new NumberFormatException("Value %s out of range for Int type.".format(l))
    l.toInt
  }
}

case object LongToInteger extends Converter {
  override def computeValue(a: Any, dstate: DState) = BigInt(asLong(a))
}

case object LongToShort extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val l = asLong(a)
    if (l > Short.MaxValue || l < Short.MinValue) throw new NumberFormatException("Value %s out of range for Short type.".format(l))
    l.toShort
  }
}

case object LongToArrayIndex extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asLong(a)
    val upperLimit = DaffodilTunableParameters.maxOccursBounds
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to an array index.".format(res))
    if (res > upperLimit) throw new NumberFormatException("Value %s out of range for an array index\nThe current (tunable) maximum is %s.".format(res, upperLimit))
    res
  }
}
case object LongToUnsignedByte extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asLong(a)
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to an unsigned byte.".format(res))
    if (res > 255) throw new NumberFormatException("Value %s out of range for unsigned byte.".format(res))
    res.toShort
  }
}
case object LongToUnsignedInt extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asLong(a)
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to an unsigned int.".format(res))
    if (res > 0xFFFFFFFFL) throw new NumberFormatException("Value %s out of range for unsigned int.".format(res))
    res
  }
}
case object LongToUnsignedShort extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asLong(a)
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to an unsigned short.".format(res))
    if (res > 65535) throw new NumberFormatException("Value %s out of range for unsigned short.".format(res))
    res.toInt
  }
}

case object LongToNonNegativeInteger extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = BigInt(asLong(a))
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to a non-negative integer.".format(res))
    res
  }
}

case object LongToUnsignedLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = BigInt(asLong(a))
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to a non-negative integer.".format(res))
    else res
  }
}

case object StringToBoolean extends Converter {
  override def computeValue(a: Any, dstate: DState) = if (a.asInstanceOf[String].length == 0) false else true
}
case object StringToDecimal extends Converter {
  override def computeValue(a: Any, dstate: DState) = BigDecimal(a.asInstanceOf[String])
}
case object StringToDouble extends Converter {
  override def computeValue(a: Any, dstate: DState) = a.asInstanceOf[String].toDouble
}
case object StringToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res =
      try {
        a.asInstanceOf[String].toLong
      } catch {
        case nfe: NumberFormatException => {
          val e = new NumberFormatException("Cannot convert to type long: " + nfe.getMessage())
          throw e
        }
      }
    res
  }
}
case object StringToUnsignedLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = BigInt(a.asInstanceOf[String])
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to an unsigned long.".format(res))
    if (res > NodeInfo.UnsignedLong.Max) throw new NumberFormatException("Value %s out of range for UnsignedLong type.".format(res))
    else res
  }
}

case object TimeToDateTime extends Converter {
  override def computeValue(a: Any, dstate: DState) = ??? // a.asInstanceOf[Time].toDateTime
}
trait XSDateTimeKind {
  val timeZoneID = "UTC"
  lazy val calendar = new ThreadLocal[Calendar] {
    override def initialValue = {
      val cal = Calendar.getInstance()
      cal.clear()
      cal.setLenient(false)
      cal.setTimeZone(new SimpleTimeZone(0, timeZoneID))
      cal
    }
  }

  def defaultFormatter: ThreadLocal[SimpleDateFormat]
  def acceptableFormats: Seq[SimpleDateFormat]

  def getNewCalendar: Calendar = calendar.get.clone().asInstanceOf[Calendar]

  protected def createCalendar(str: String, inFormat: SimpleDateFormat,
    fncName: String, toType: String): DFDLCalendar

  def matchFormat(str: String, fncName: String, toType: String): DFDLCalendar = {

    acceptableFormats.foreach(f => {
      val inFormat = f
      inFormat.setCalendar(getNewCalendar)
      try {
        val cal = createCalendar(str, inFormat, fncName, toType)
        // Here we've successfully created a calendar using the expected format
        // denoted by 'inFormat'. Return the calendar.
        return cal
      } catch {
        case e: IllegalArgumentException => /* Format failed, continue trying to match other formats */
      }

    })
    // All acceptable formats failed
    throw new NumberFormatException("Failed to convert \"%s\" to %s.".format(str, toType))
  }
}

trait DateTimeFormatters {
  lazy val defaultFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxxxx")
      format
    }
  }

  lazy val withFractNoTimeZoneFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("uuuu-MM-dd'T'HH:mm:ss.SSSSSS")
      format
    }
  }

  lazy val withTimeZoneNoFractFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("uuuu-MM-dd'T'HH:mm:ssxxxxx")
      format
    }
  }

  lazy val noTimeZoneNoFractFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("uuuu-MM-dd'T'HH:mm:ss")
      format
    }
  }

  lazy val withTimeZoneFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("uuuu-MM-ddxxxxx")
      format
    }
  }

  lazy val dateOnlyFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("uuuu-MM-dd")
      format
    }
  }
}

trait DateFormatters {
  lazy val defaultFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("uuuu-MM-ddxxxxx")
      format
    }
  }
  lazy val withoutTimezoneFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("uuuu-MM-dd")
      format
    }
  }
}

trait TimeFormatters {
  lazy val defaultFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("HH:mm:ss.SSSSSSxxxxx")
      format
    }
  }

  lazy val noTimeZoneFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("HH:mm:ss.SSSSSS")
      format
    }
  }

  lazy val noTimeZoneNoFractFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("HH:mm:ss")
      format
    }
  }

  lazy val withTimeZoneNoFractFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("HH:mm:ssxxxxx")
      format
    }
  }
}

case object ToDate extends Converter with XSDateTimeKind with DateFormatters {
  val name = "ToDate"

  def acceptableFormats = Seq(defaultFormatter.get, withoutTimezoneFormatter.get)

  protected def createCalendar(str: String, inFormat: SimpleDateFormat, fncName: String, toType: String): DFDLCalendar = {
    Conversion.stringToDFDLDate(str, inFormat, fncName, toType)
  }

  override def computeValue(a: Any, dstate: DState): Any = {
    val result = a match {
      case cal: DFDLDateTime => cal.toDate
      case cal: DFDLDate => cal
      case str: String => matchFormat(str, name, "xs:date")
      case _ => throw new NumberFormatException("xs:date only accepts String, Date or DateTime objects.")
    }
    result
  }
}
case object ToDateTime extends Converter
  with XSDateTimeKind with DateTimeFormatters {
  val name = "ToDateTime"

  def acceptableFormats = Seq(defaultFormatter.get, withFractNoTimeZoneFormatter.get,
    noTimeZoneNoFractFormatter.get, withTimeZoneFormatter.get,
    withTimeZoneNoFractFormatter.get, dateOnlyFormatter.get)

  protected def createCalendar(str: String, inFormat: SimpleDateFormat,
    fncName: String, toType: String): DFDLCalendar = {
    Conversion.stringToDFDLDateTime(str, inFormat, fncName, toType)
  }

  override def computeValue(a: Any, dstate: DState): Any = {
    val result = a match {
      case cal: DFDLDateTime => cal
      case cal: DFDLDate => cal.toDateTime
      case str: String => matchFormat(str, name, "xs:dateTime")
      case _ => throw new NumberFormatException("xs:dateTime only accepts String, Date or DateTime objects.")
    }
    result
  }

}
case object XSHexBinary extends Converter with HexBinaryKind {
  val name = "XSHexBinary"

  override def computeValue(a: Any, dstate: DState): Any = {
    // Apparently an invariant at DPath.scala 156 wants this to be
    // Array[Byte]
    //
    // Check for:
    // 1. Even number of characters
    // 2. Valid hex (0-9 A-F)
    val arr = a match {
      case s: String => hexStringToByteArray(s)
      case hb: Array[Byte] => hb
      case x => throw new NumberFormatException("%s cannot be cast to dfdl:hexBinary\ndfdl:hexBinary received an unrecognized type! Must be String or HexBinary.".format(x.toString))
    }

    arr
  }
}

case object ToTime extends Converter with XSDateTimeKind with TimeFormatters {
  val name = "ToTime"

  def acceptableFormats = Seq(defaultFormatter.get, noTimeZoneFormatter.get,
    noTimeZoneNoFractFormatter.get, withTimeZoneNoFractFormatter.get)

  protected def createCalendar(str: String, inFormat: SimpleDateFormat,
    fncName: String, toType: String): DFDLCalendar = {
    Conversion.stringToDFDLTime(str, inFormat, fncName, toType)
  }

  override def computeValue(a: Any, dstate: DState): Any = {
    val result = a match {
      case cal: DFDLDateTime => cal.toTime
      case cal: DFDLTime => cal
      case str: String => matchFormat(str, name, "xs:time")
      case _ => throw new NumberFormatException("xs:time only accepts String, DateTime or Time objects.")
    }
    result
  }

}
trait ToString extends Converter {
  override def computeValue(a: Any, dstate: DState) = a.toString
}
case object NumericToString extends ToString
case object DateTimeToString extends ToString
case object HexBinaryToString extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val bytes = a.asInstanceOf[Array[Byte]]
    val hex = Misc.bytes2Hex(bytes)
    hex
  }
}
case object HexStringToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res =
      try {
        val str = a.asInstanceOf[String]
        java.lang.Long.parseLong(str, 16)
      } catch {
        case nfe: NumberFormatException => {
          val e = new NumberFormatException("Cannot convert to type long: " + nfe.getMessage())
          throw e
        }
      }
    res
  }
}
case object HexStringToUnsignedLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res =
      try {
        val str = a.asInstanceOf[String]
        BigInt(str, 16)
      } catch {
        case nfe: NumberFormatException => {
          val e = new NumberFormatException("Cannot convert to type unsignedLong: " + nfe.getMessage())
          throw e
        }
      }
    res
  }
}
case object BigIntToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asBigInt(a)
    if (res < Long.MinValue || res > Long.MaxValue) throw new NumberFormatException("Value %s out of range for Long type.".format(res))
    res.toLong
  }
}
case object IntToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = asInt(a).toLong
}
case object UnsignedLongToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asBigDecimal(a)
    if (res < Long.MinValue || res > Long.MaxValue) throw new NumberFormatException("Value %s out of range for Long type.".format(res))
    res.toLong
  }
}
case object UnsignedIntToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    // Assert.invariant(a.isInstanceOf[Long])
    asLong(a)
  }
}
case object ArrayIndexToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    asLong(a)
  }
}
case object ShortToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = asInt(a).toLong
}
case object UnsignedShortToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = asInt(a).toLong
}
case object ByteToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = asInt(a).toLong
}
case object UnsignedByteToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = asInt(a).toLong
}

case object PlusDecimal extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigDecimal(v1) + asBigDecimal(v2) }
}
case object MinusDecimal extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigDecimal(v1) - asBigDecimal(v2) }
}
case object TimesDecimal extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigDecimal(v1) * asBigDecimal(v2) }
}
case object DivDecimal extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigDecimal(v1) / asBigDecimal(v2) }
}
case object IDivDecimal extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigDecimal(v1) / asBigDecimal(v2) }
}
case object ModDecimal extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigDecimal(v1) % asBigDecimal(v2) }
}

case object PlusInteger extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigInt(v1) + asBigInt(v2) }
}
case object MinusInteger extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigInt(v1) - asBigInt(v2) }
}
case object TimesInteger extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigInt(v1) * asBigInt(v2) }
}
case object DivInteger extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigInt(v1) / asBigInt(v2) }
}
case object IDivInteger extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigInt(v1) / asBigInt(v2) }
}
case object ModInteger extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigInt(v1) % asBigInt(v2) }
}

case object PlusNonNegativeInteger extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigInt(v1) + asBigInt(v2) }
}
case object MinusNonNegativeInteger extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigInt(v1) - asBigInt(v2) }
}
case object TimesNonNegativeInteger extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigInt(v1) * asBigInt(v2) }
}
case object DivNonNegativeInteger extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigInt(v1) / asBigInt(v2) }
}
case object IDivNonNegativeInteger extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigInt(v1) / asBigInt(v2) }
}
case object ModNonNegativeInteger extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigInt(v1) % asBigInt(v2) }
}

case object PlusUnsignedLong extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigInt(v1) + asBigInt(v2) }
}
case object MinusUnsignedLong extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigInt(v1) - asBigInt(v2) }
}
case object TimesUnsignedLong extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigInt(v1) * asBigInt(v2) }
}
case object DivUnsignedLong extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigInt(v1) / asBigInt(v2) }
}
case object IDivUnsignedLong extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigInt(v1) / asBigInt(v2) }
}
case object ModUnsignedLong extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asBigInt(v1) % asBigInt(v2) }
}

case object PlusLong extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asLong(v1) + asLong(v2) }
}
case object MinusLong extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asLong(v1) - asLong(v2) }
}
case object TimesLong extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asLong(v1) * asLong(v2) }
}
case object DivLong extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asLong(v1) / asLong(v2) }
}
case object IDivLong extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asLong(v1) / asLong(v2) }
}
case object ModLong extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asLong(v1) % asLong(v2) }
}

case object PlusUnsignedInt extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asLong(v1) + asLong(v2) }
}
case object MinusUnsignedInt extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asLong(v1) - asLong(v2) }
}
case object TimesUnsignedInt extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asLong(v1) * asLong(v2) }
}
case object DivUnsignedInt extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asLong(v1) / asLong(v2) }
}
case object IDivUnsignedInt extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asLong(v1) / asLong(v2) }
}
case object ModUnsignedInt extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asLong(v1) % asLong(v2) }
}

case object PlusInt extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asInt(v1) + asInt(v2) }
}
case object MinusInt extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asInt(v1) - asInt(v2) }
}
case object TimesInt extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asInt(v1) * asInt(v2) }
}
case object DivInt extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asInt(v1) / asInt(v2) }
}
case object IDivInt extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asInt(v1) / asInt(v2) }
}
case object ModInt extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asInt(v1) % asInt(v2) }
}

case object PlusUnsignedShort extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asInt(v1) + asInt(v2) }
}
case object MinusUnsignedShort extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asInt(v1) - asInt(v2) }
}
case object TimesUnsignedShort extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asInt(v1) * asInt(v2) }
}
case object DivUnsignedShort extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asInt(v1) / asInt(v2) }
}
case object IDivUnsignedShort extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asInt(v1) / asInt(v2) }
}
case object ModUnsignedShort extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asInt(v1) % asInt(v2) }
}

case object PlusShort extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asShort(v1) + asShort(v2) }
}
case object MinusShort extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asShort(v1) - asShort(v2) }
}
case object TimesShort extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asShort(v1) * asShort(v2) }
}
case object DivShort extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asShort(v1) / asShort(v2) }
}
case object IDivShort extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asShort(v1) / asShort(v2) }
}
case object ModShort extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asShort(v1) % asShort(v2) }
}

case object PlusUnsignedByte extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asShort(v1) + asShort(v2) }
}
case object MinusUnsignedByte extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asShort(v1) - asShort(v2) }
}
case object TimesUnsignedByte extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asShort(v1) * asShort(v2) }
}
case object DivUnsignedByte extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asShort(v1) / asShort(v2) }
}
case object IDivUnsignedByte extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asShort(v1) / asShort(v2) }
}
case object ModUnsignedByte extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asShort(v1) % asShort(v2) }
}

case object PlusByte extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asByte(v1) + asByte(v2) }
}
case object MinusByte extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asByte(v1) - asByte(v2) }
}
case object TimesByte extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asByte(v1) * asByte(v2) }
}
case object DivByte extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asByte(v1) / asByte(v2) }
}
case object IDivByte extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asByte(v1) / asByte(v2) }
}
case object ModByte extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asByte(v1) % asByte(v2) }
}

case object PlusFloat extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asFloat(v1) + asFloat(v2) }
}
case object MinusFloat extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asFloat(v1) - asFloat(v2) }
}
case object TimesFloat extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asFloat(v1) * asFloat(v2) }
}
case object DivFloat extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asFloat(v1) / asFloat(v2) }
}
case object IDivFloat extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asFloat(v1) / asFloat(v2) }
}
case object ModFloat extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asFloat(v1) % asFloat(v2) }
}

case object PlusDouble extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asDouble(v1) + asDouble(v2) }
}
case object MinusDouble extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asDouble(v1) - asDouble(v2) }
}
case object TimesDouble extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asDouble(v1) * asDouble(v2) }
}
case object DivDouble extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asDouble(v1) / asDouble(v2) }
}
case object IDivDouble extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asDouble(v1) / asDouble(v2) }
}
case object ModDouble extends NumericOp {
  def operate(v1: Any, v2: Any): Any = { asDouble(v1) % asDouble(v2) }
}

case object StringToNonEmptyString extends RecipeOp {
  override def run(dstate: DState) {
    val current = dstate.currentValue.asInstanceOf[String]
    if (current.length == 0)
      throw new IllegalArgumentException("String value may not be empty.")
  }
}

case object DAFError extends RecipeOp {
  override def run(dstate: DState) {
    val ie = dstate.pstate.infoset.asInstanceOf[DIElement]
    val pe = new ParseError(One(ie.runtimeData.schemaFileLocation), One(dstate.pstate), "The error function was called.")
    throw pe
  }
}

case object LT_Decimal extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigDecimal(v1) < asBigDecimal(v2) }
}
case object GT_Decimal extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigDecimal(v1) > asBigDecimal(v2) }
}
case object LE_Decimal extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigDecimal(v1) <= asBigDecimal(v2) }
}
case object GE_Decimal extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigDecimal(v1) >= asBigDecimal(v2) }
}
case object LT_Integer extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) < asBigInt(v2) }
}
case object GT_Integer extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) > asBigInt(v2) }
}
case object LE_Integer extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) <= asBigInt(v2) }
}
case object GE_Integer extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) >= asBigInt(v2) }
}
case object LT_NonNegativeInteger extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) < asBigInt(v2) }
}
case object GT_NonNegativeInteger extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) > asBigInt(v2) }
}
case object LE_NonNegativeInteger extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) <= asBigInt(v2) }
}
case object GE_NonNegativeInteger extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) >= asBigInt(v2) }
}
case object LT_UnsignedLong extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) < asBigInt(v2) }
}
case object GT_UnsignedLong extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) > asBigInt(v2) }
}
case object LE_UnsignedLong extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) <= asBigInt(v2) }
}
case object GE_UnsignedLong extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) >= asBigInt(v2) }
}
case object LT_Long extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asLong(v1) < asLong(v2) }
}
case object GT_Long extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asLong(v1) > asLong(v2) }
}
case object LE_Long extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asLong(v1) <= asLong(v2) }
}
case object GE_Long extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asLong(v1) >= asLong(v2) }
}
case object LT_UnsignedInt extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asLong(v1) < asLong(v2) }
}
case object GT_UnsignedInt extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asLong(v1) > asLong(v2) }
}
case object LE_UnsignedInt extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asLong(v1) <= asLong(v2) }
}
case object GE_UnsignedInt extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asLong(v1) >= asLong(v2) }
}
case object LT_Int extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asInt(v1) < asInt(v2) }
}
case object GT_Int extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asInt(v1) > asInt(v2) }
}
case object LE_Int extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asInt(v1) <= asInt(v2) }
}
case object GE_Int extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asInt(v1) >= asInt(v2) }
}
case object LT_UnsignedShort extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asInt(v1) < asInt(v2) }
}
case object GT_UnsignedShort extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asInt(v1) > asInt(v2) }
}
case object LE_UnsignedShort extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asInt(v1) <= asInt(v2) }
}
case object GE_UnsignedShort extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asInt(v1) >= asInt(v2) }
}
case object LT_Short extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asShort(v1) < asShort(v2) }
}
case object GT_Short extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asShort(v1) > asShort(v2) }
}
case object LE_Short extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asShort(v1) <= asShort(v2) }
}
case object GE_Short extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asShort(v1) >= asShort(v2) }
}
case object LT_UnsignedByte extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asShort(v1) < asShort(v2) }
}
case object GT_UnsignedByte extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asShort(v1) > asShort(v2) }
}
case object LE_UnsignedByte extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asShort(v1) <= asShort(v2) }
}
case object GE_UnsignedByte extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asShort(v1) >= asShort(v2) }
}
case object LT_Byte extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asByte(v1) < asByte(v2) }
}
case object GT_Byte extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asByte(v1) > asByte(v2) }
}
case object LE_Byte extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asByte(v1) <= asByte(v2) }
}
case object GE_Byte extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asByte(v1) >= asByte(v2) }
}
case object LT_Float extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asFloat(v1) < asFloat(v2) }
}
case object GT_Float extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asFloat(v1) > asFloat(v2) }
}
case object LE_Float extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asFloat(v1) <= asFloat(v2) }
}
case object GE_Float extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asFloat(v1) >= asFloat(v2) }
}
case object LT_Double extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asDouble(v1) < asDouble(v2) }
}
case object GT_Double extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asDouble(v1) > asDouble(v2) }
}
case object LE_Double extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asDouble(v1) <= asDouble(v2) }
}
case object GE_Double extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asDouble(v1) >= asDouble(v2) }
}

