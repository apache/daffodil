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
import edu.illinois.ncsa.daffodil.dsom.TypeConversions
import edu.illinois.ncsa.daffodil.dsom.RuntimeSchemaDefinitionError 
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

class DPathRecipe(val ops: RecipeOp*) extends Serializable with PreSerialization {

  def this(ops: List[RecipeOp]) = this(ops.toArray: _*)

  override def toString = toXML.toString

  def toXML = <DPathRecipe>{ ops.map { _.toXML } }</DPathRecipe>

  def runExpression(pstate: PState) {
    val dstate = pstate.dstate
    dstate.setCurrentNode(pstate.infoset.asInstanceOf[DINode])
    dstate.setVMap(pstate.variableMap)
    dstate.setPState(pstate)
    dstate.resetValue
    run(dstate)
  }

  /**
   * Must be called before serialization, and before evaluating
   * the expression on a real infoset.
   */
  override def preSerialization: Seq[ElementRuntimeData] = preSerializedERDs

  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = {
    preSerialization
    out.defaultWriteObject()
  }

  private lazy val preSerializedERDs = {
    ops.flatMap { _.preSerialization }
  }

  /**
   * Used at compilation time to evaluate expressions that
   * are known to be constants.
   */
  def runExpressionForConstant(context: Option[SchemaFileLocatable]): Option[Any] = {

    //
    // we use a special dummy dstate here that errors out via throw
    // if the evaluation tries to get a processor state or node.
    //
    val dstate = new DState {

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
    val isConstant: Boolean =
      try {
        run(dstate)
        // it ran, so must have produced a constant value
        val v = dstate.currentValue
        Assert.invariant(v != null)
        dstate.setCurrentValue(v)
        true
      } catch {
        case e: java.lang.IllegalStateException =>
          false // useful place for breakpoint
        case e: java.lang.NumberFormatException => throw new SchemaDefinitionError(Some(context.get.schemaFileLocation), None, e.getMessage)
        case e: java.lang.IndexOutOfBoundsException => false
        case e: java.lang.IllegalArgumentException => false
        case e: SchemaDefinitionDiagnosticBase => throw new SchemaDefinitionError(Some(context.get.schemaFileLocation), None, e.getMessage)
        case e: ProcessingError => throw new SchemaDefinitionError(Some(context.get.schemaFileLocation), None, e.getMessage)
        case th: Throwable =>
          throw th
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

/**
 * expression evaluation side-effects this state block.
 */
case class DState() extends AsIntMixin { // extends NotSerializable

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
  // checking expressions to see if they are constants.
  def selfMove(): Unit = {}
  def fnExists(): Unit = {}
}

trait AsIntMixin {
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

sealed abstract class RecipeOp extends AsIntMixin with Serializable with PreSerialization { // extends TypeConversions {

  def run(dstate: DState): Unit

  protected def subRecipes: Seq[DPathRecipe] = Nil

  override def preSerialization: Seq[ElementRuntimeData] = {
    subRecipes.flatMap { _.preSerialization }
  }

  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = {
    preSerialization
    out.defaultWriteObject()
  }

  def toXML(s: String): scala.xml.Node = toXML(new scala.xml.Text(s))

  def toXML(children: scala.xml.Node*): scala.xml.Node = toXML(children.toSeq)

  def toXML(children: scala.xml.NodeSeq): scala.xml.Node = {
    val name = Misc.getNameFromClass(this)
    scala.xml.Elem(null, name, scala.xml.Null, scala.xml.TopScope, children.isEmpty, children: _*)
  }

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

abstract class RecipeOpWithSubRecipes(recipes: List[DPathRecipe]) extends RecipeOp {

  override def subRecipes: List[DPathRecipe] = recipes

  def this(recipes: DPathRecipe*) = this(recipes.toList)

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
 * Some RecipeOps need elementRuntimeData as they navigate down
 * the infoset.
 *
 * However, there's this circularity problem. The expression compiler
 * is used to determine if the values of expressions are constants,
 * and that is done by compiling them and evaluating them at compile
 * time. Then some compiled expressions want to become part of the
 * elementRuntimeData, and we're chasing our tail.
 *
 * The fix for this is that evaluation to determine if an expression
 * is constant is done in a mode where no elementRuntimeData objects
 * have been supplied yet.
 *
 * When we really want to evaluate the expression, or serialize it out
 * to a file, then we must run the preSerialization method, which
 * then extracts the real elementRuntimeData object, and serializes
 * that one, not the schema component from which it was derived.
 */
trait RecipeOpWithERD
  extends RecipeOp {

  protected def info: DPathElementCompileInfo

  private var _erd: ElementRuntimeData = null

  final override lazy val preSerialization = {
    _erd = info.elementRuntimeData
    _erd +: super.preSerialization
  }

  final protected def erd = {
    if (_erd == null) throw new IllegalStateException("no runtime data")
    _erd
  }
}
/**
 * Down to a non-array element. Can be optional or scalar.
 */
case class DownElement(@transient infoArg: DPathElementCompileInfo) extends RecipeOpWithERD with Serializable with PreSerialization  {
  @transient final override val info = infoArg

  override def run(dstate: DState) {
    val now = dstate.currentComplex
    // TODO PE ? if doesn't exist should be a processing error.
    // It will throw and so will be a PE, but may be poor diagnostic.
    dstate.setCurrentNode(now.getChild(erd).asInstanceOf[DIElement])
  }

  override def toXML = {
    preSerialization
    toXML(erd.name)
  }
  
  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = {
    preSerialization
    out.defaultWriteObject()
  }

}

/**
 * Move down to an occurrence of an array element.
 */
case class DownArrayOccurrence(@transient infoArg: DPathElementCompileInfo, indexRecipe: DPathRecipe)
  extends RecipeOpWithSubRecipes(indexRecipe)
  with RecipeOpWithERD {

  @transient final override val info = infoArg

  override def run(dstate: DState) {
    val savedCurrentElement = dstate.currentComplex
    indexRecipe.run(dstate)
    val index = dstate.index
    Assert.invariant(dstate.index > 0) // TODO PE?
    val arr = savedCurrentElement.getChildArray(erd)
    Assert.invariant(arr.isDefined) // TODO PE?
    val occurrence = arr.get.getOccurrence(index) // will throw on out of bounds
    dstate.setCurrentNode(occurrence.asInstanceOf[DIElement])
  }

  override def toXML = {
    preSerialization
    toXML(new scala.xml.Text(info.name) ++ indexRecipe.toXML)
  }

  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = {
    preSerialization
    out.defaultWriteObject()
  }
}

/*
 * down to an array object containing all occurrences
 */
case class DownArray(@transient infoArg: DPathElementCompileInfo) extends RecipeOpWithERD with Serializable with PreSerialization {

  @transient final override val info = infoArg

  override def run(dstate: DState) {
    val now = dstate.currentComplex
    val arr = now.getChildArray(erd)
    Assert.invariant(arr.isDefined) // TODO PE?
    dstate.setCurrentNode(arr.get.asInstanceOf[DIArray])
  }

  override def toXML = {
    preSerialization
    toXML(erd.name)
  }
  
  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = {
    preSerialization
    out.defaultWriteObject()
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

case class DFDLCheckConstraints(recipe: DPathRecipe) extends RecipeOpWithSubRecipes(recipe) {
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

case class DFDLDecodeDFDLEntities(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = {
    val dfdlString = EntityReplacer { _.replaceAll(str.asInstanceOf[String], None) }
    dfdlString
  }
}

case class DFDLEncodeDFDLEntities(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
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

case class DFDLContainsDFDLEntities(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) =
    EntityReplacer { _.hasDfdlEntity(str.asInstanceOf[String]) }
}

case class DFDLTestBit(dataRecipe: DPathRecipe, bitPos1bRecipe: DPathRecipe)
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

case class DFDLSetBits(bitRecipes: List[DPathRecipe]) extends RecipeOpWithSubRecipes(bitRecipes) {

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

case class IF(predRecipe: DPathRecipe, thenPartRecipe: DPathRecipe, elsePartRecipe: DPathRecipe)
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
  def left: DPathRecipe
  def right: DPathRecipe
  override def subRecipes: Seq[DPathRecipe] = Seq(left, right)

  override def toXML: scala.xml.Node = toXML(new scala.xml.Text(op), left.toXML, right.toXML)
}

case class NumberCompareOperator(cop: NumberCompareOp, left: DPathRecipe, right: DPathRecipe)
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

trait NumberCompareOp extends AsIntMixin {
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

case class EqualityCompareOp(op: String, left: DPathRecipe, right: DPathRecipe)
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

case class BooleanOp(op: String, left: DPathRecipe, right: DPathRecipe)
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
case class NegateOp(recipe: DPathRecipe) extends RecipeOpWithSubRecipes(recipe) {
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

case class FNDateTime(recipes: List[DPathRecipe]) extends FNTwoArgs(recipes) {
  val name = "FNDateTime"
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

    val finalCal = Conversion.calendarToDFDLCalendar(newCal, "yyyy-MM-dd'T'HH:mm:ss.SSSSSSxxxxx", dstate, name, "DateTime")
    finalCal
  }
}

case class FNRoundHalfToEven(recipeNum: DPathRecipe, recipePrecision: DPathRecipe)
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
      // val mc = new java.math.MathContext(precision, java.math.RoundingMode.HALF_EVEN)
      val rounded = bd.setScale(precision, BigDecimal.RoundingMode.HALF_EVEN)
      rounded
    }
    dstate.setCurrentValue(value)
  }
}

case class FNNot(recipe: DPathRecipe, argType: NodeInfo.Kind = null) extends FNOneArg(recipe, NodeInfo.Boolean) {
  override def computeValue(value: Any, dstate: DState) = !(value.asInstanceOf[Boolean])
}

case class FNEndsWith(recipes: List[DPathRecipe]) extends FNTwoArgs(recipes) {
  // println(recipes)
  override def computeValue(arg1: Any, arg2: Any, dstate: DState) = {
    val arg1s = arg1.asInstanceOf[String]
    val arg2s = arg2.asInstanceOf[String]
    val res = arg1s.endsWith(arg2s)
    res
  }
}

case class FNNilled(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, NodeInfo.Nillable) {
  override def computeValue(value: Any, dstate: DState) = value.asInstanceOf[DIElement].isNilled
}

case class FNExists(recipe: DPathRecipe, argType: NodeInfo.Kind) extends RecipeOpWithSubRecipes(recipe) {
  override def run(dstate: DState) {
    dstate.fnExists() // hook so we can insist this is non-constant at compile time.
    val exists =
      try {
        recipe.run(dstate)
        true
      } catch {
        // catch exceptions indicating a node doesn't exist.
        //
        // TODO: there is some risk here of masking SDEs.
        // Should recipes and the infoset operations they call
        // throw a distinct exception type that can 
        // be caught here, and if not caught here converts
        // into a regular SDE or PE?
        //
        case e: java.lang.IllegalStateException => false
        case e: java.lang.NumberFormatException => false
        case e: java.lang.IndexOutOfBoundsException => false
        case e: java.lang.IllegalArgumentException => false
        case e: SchemaDefinitionDiagnosticBase => false
        case e: ProcessingError => false
      }
    dstate.setCurrentValue(exists)
  }

  override def toXML = toXML(recipe.toXML)

}

case class FNCeiling(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) with TypeConversions {
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

case class FNFloor(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) with TypeConversions {
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

case class FNRound(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
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

case class FNAbs(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
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

case class FNStringLength(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = str.asInstanceOf[String].length.toLong
}

case class FNLowerCase(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = str.asInstanceOf[String].toLowerCase
}

case class FNUpperCase(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = str.asInstanceOf[String].toUpperCase
}

case class FNConcat(recipes: List[DPathRecipe]) extends FNArgsList(recipes) {
  override def computeValue(values: List[Any], dstate: DState) = values.mkString
}

// No such function in DFDL.
//case class FNStringJoin(recipes: List[DPathRecipe]) extends FNTwoArgs(recipes) {
//  override def computeValue(arg1: Any, arg2: Any, dstate: DState) = {
//    val values = arg1.asInstanceOf[List[String]]
//    val sep = arg2.asInstanceOf[String]
//    values.mkString(sep)
//  }
//}

case class FNSubstring2(recipes: List[DPathRecipe]) extends FNTwoArgs(recipes) {
  override def computeValue(arg1: Any, arg2: Any, dstate: DState) = {
    val sourceString = arg1.asInstanceOf[String]
    val startPos = asDouble(arg2)
    sourceString.substring(startPos.toInt - 1)
  }
}

case class FNSubstring3(recipes: List[DPathRecipe]) extends FNThreeArgs(recipes) {
  override def computeValue(arg1: Any, arg2: Any, arg3: Any, dstate: DState) = {
    val sourceString = arg1.asInstanceOf[String]
    val startPos = asDouble(arg2).toInt - 1 //adjust to zero-based
    val length = asDouble(arg3).toInt
    sourceString.substring(startPos, length)
  }
}

abstract class FNOneArg(recipe: DPathRecipe, argType: NodeInfo.Kind) extends RecipeOpWithSubRecipes(recipe) {
  override def run(dstate: DState) {
    recipe.run(dstate)
    val arg = dstate.currentValue
    dstate.setCurrentValue(computeValue(arg, dstate))
  }

  override def toXML = toXML(recipe.toXML)

  def computeValue(str: Any, dstate: DState): Any
}

abstract class FNTwoArgs(recipes: List[DPathRecipe]) extends RecipeOpWithSubRecipes(recipes) {
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

abstract class FNThreeArgs(recipes: List[DPathRecipe]) extends RecipeOpWithSubRecipes(recipes) {
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

case class FNYearFromDateTime(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = str.asInstanceOf[DFDLCalendar].getField(Calendar.YEAR)
}
case class FNMonthFromDateTime(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = str.asInstanceOf[DFDLCalendar].getField(Calendar.MONTH) + 1 // JAN 0
}
case class FNDayFromDateTime(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = str.asInstanceOf[DFDLCalendar].getField(Calendar.DAY_OF_MONTH)
}
case class FNHoursFromDateTime(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = str.asInstanceOf[DFDLCalendar].getField(Calendar.HOUR_OF_DAY)
}
case class FNMinutesFromDateTime(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = str.asInstanceOf[DFDLCalendar].getField(Calendar.MINUTE)
}
case class FNSecondsFromDateTime(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = str.asInstanceOf[DFDLCalendar].getField(Calendar.SECOND)
}

case class FNYearFromDate(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = str.asInstanceOf[DFDLCalendar].getField(Calendar.YEAR)
}
case class FNMonthFromDate(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = str.asInstanceOf[DFDLCalendar].getField(Calendar.MONTH) + 1 // JAN 0
}
case class FNDayFromDate(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = str.asInstanceOf[DFDLCalendar].getField(Calendar.DAY_OF_MONTH)
}
case class FNHoursFromTime(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = str.asInstanceOf[DFDLCalendar].getField(Calendar.HOUR_OF_DAY)
}
case class FNMinutesFromTime(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = str.asInstanceOf[DFDLCalendar].getField(Calendar.MINUTE)
}
case class FNSecondsFromTime(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = str.asInstanceOf[DFDLCalendar].getField(Calendar.SECOND)
}

abstract class FNArgsList(recipes: List[DPathRecipe]) extends RecipeOpWithSubRecipes(recipes) {
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

case class XSInt(recipe: DPathRecipe) extends RecipeOpWithSubRecipes(recipe) {
  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    recipe.run(dstate)
    val basicValue = dstate.currentValue
    val value = asInt(basicValue)
    dstate.setCurrentValue(value)
  }
}

case class XSString(recipe: DPathRecipe, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(value: Any, dstate: DState) = value.toString
}

case class XSDateTime(recipe: DPathRecipe, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) with XSDateTimeKind with DateTimeFormatters {

  val name = "XSDateTime"

  override def computeValue(a: Any, dstate: DState): Any = {
    val defaultFormat = defaultFormatter.get
    defaultFormat.setCalendar(getNewCalendar)
    Conversion.stringToDFDLCalendar(a.toString, defaultFormat, defaultFormat, name, "DateTime") match {
      case Left(failure1) => {
        val dateTimeWithFractNoTimeZone = withFractNoTimeZoneFormatter.get
        dateTimeWithFractNoTimeZone.setCalendar(getNewCalendar)
        Conversion.stringToDFDLCalendar(a.toString, dateTimeWithFractNoTimeZone, defaultFormat, name, "DateTime") match {
          case Left(failure2) => {
            val dateTimeWithTimeZoneNoFract = withTimeZoneNoFractFormatter.get
            dateTimeWithTimeZoneNoFract.setCalendar(getNewCalendar)
            Conversion.stringToDFDLCalendar(a.toString, dateTimeWithTimeZoneNoFract, defaultFormat, name, "DateTime") match {
              case Left(failure3) => {
                val dateTimeNoTimeZoneNoFract = noTimeZoneNoFractFormatter.get
                dateTimeNoTimeZoneNoFract.setCalendar(getNewCalendar)
                Conversion.stringToDFDLCalendar(a.toString, dateTimeNoTimeZoneNoFract, defaultFormat, name, "DateTime") match {
                  case Left(failure4) => {
                    val dateWithTimeZoneFormat = withTimeZoneFormatter.get
                    dateWithTimeZoneFormat.setCalendar(getNewCalendar)
                    Conversion.stringToDFDLCalendar(a.toString, dateWithTimeZoneFormat, defaultFormat, name, "DateTime") match {
                      case Left(failure5) => {
                        val dateOnlyFormat = dateOnlyFormatter.get
                        dateOnlyFormat.setCalendar(getNewCalendar)
                        Conversion.stringToDFDLCalendar(a.toString, dateOnlyFormat, defaultFormat, name, "DateTime") match {
                          case Left(failure6) => {
                            dstate.pstate.SDE("XSDate failed due to: " + failure1 + "\n" + failure2 +
                              "\n" + failure3 + "\n" + failure4 + "\n" + failure5 + "\n" + failure6)
                          }
                          case Right(cal) => return cal
                        }
                      }
                      case Right(cal) => return cal
                    }
                  }
                  case Right(cal) => return cal
                }
              }
              case Right(cal) => return cal
            }
          }
          case Right(cal) => return cal
        }
      }
      case Right(cal) => return cal
    }
  }
}

case class XSDate(recipe: DPathRecipe, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) with XSDateTimeKind with DateFormatters {
  val name = "XSDate"

  override def computeValue(a: Any, dstate: DState): Any = {
    val defaultFormat = defaultFormatter.get
    defaultFormat.setCalendar(getNewCalendar)

    Conversion.stringToDFDLCalendar(a.toString, defaultFormat, defaultFormat, name, "Date") match {
      case Left(failure1) => {
        val formatWithoutTimezone = withoutTimezoneFormatter.get
        formatWithoutTimezone.setCalendar(getNewCalendar)

        Conversion.stringToDFDLCalendar(a.toString, formatWithoutTimezone, defaultFormat, name, "Date") match {
          case Left(failure2) => dstate.pstate.SDE("XSDate failed due to: " + failure1 + " and " + failure2)
          case Right(cal) => return cal
        }
      }
      case Right(cal) => return cal
    }
  }
}

case class XSTime(recipe: DPathRecipe, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) with XSDateTimeKind with TimeFormatters {
  val name = "XSTime"

  override def computeValue(a: Any, dstate: DState): Any = {
    val defaultFormat = defaultFormatter.get
    defaultFormat.setCalendar(getNewCalendar)
    Conversion.stringToDFDLCalendar(a.toString, defaultFormat, defaultFormat, name, "Time") match {
      case Left(failure1) => {
        val formatNoTimeZone = noTimeZoneFormatter.get
        formatNoTimeZone.setCalendar(getNewCalendar)
        Conversion.stringToDFDLCalendar(a.toString, formatNoTimeZone, defaultFormat, name, "Time") match {
          case Left(failure2) => {
            val formatNoTimeZoneNoFractional = noTimeZoneNoFractFormatter.get
            formatNoTimeZoneNoFractional.setCalendar(getNewCalendar)
            Conversion.stringToDFDLCalendar(a.toString, formatNoTimeZoneNoFractional, defaultFormat, name, "Time") match {
              case Left(failure3) => {
                val formatWithTimeZoneNoFractional = withTimeZoneNoFractFormatter.get
                formatWithTimeZoneNoFractional.setCalendar(getNewCalendar)
                Conversion.stringToDFDLCalendar(a.toString, formatWithTimeZoneNoFractional, defaultFormat, name, "Time") match {
                  case Left(failure4) => dstate.pstate.SDE("XSTime failed due to " + failure1 + ", " + failure2 + ", " + failure3 + " and " + failure4)
                  case Right(cal) => return cal
                }
              }
              case Right(cal) => return cal
            }
          }
          case Right(cal) => return cal
        }
      }
      case Right(cal) => return cal
    }
  }
}

trait HexBinaryKind {
  val hexPattern = """([0-9A-Fa-f]+)""".r

  /**
   * http://travisdazell.blogspot.com/2012/11/converting-hex-string-to-byte-array-in.html
   */
  def hexStringToByteArray(str: String): Either[String, Array[Byte]] = {
    val len = str.length

    if ((len % 2) != 0)
      return Left("Failed to evaluate expression: A hexBinary value must contain an even number of characters.")

    str match {
      case hexPattern(value) => // Correct, continue
      case _ => return Left("Failed to evaluate expression: Invalid hexadecimal digit.")
    }

    val arr = new Array[Byte](len / 2)
    var i = 0
    while (i < len) {
      val byte = (Character.digit(str.charAt(i), 16) << 4) + (Character.digit(str.charAt(i + 1), 16))
      arr(i / 2) = byte.asInstanceOf[Byte]
      i += 2
    }
    return Right(arr)
  }
}

case class XSHexBinary(recipe: DPathRecipe, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) with HexBinaryKind {
  val name = "XSHexBinary"

  override def computeValue(a: Any, dstate: DState): Any = {
    // Check for:
    // 1. Even number of characters
    // 2. Valid hex (0-9 A-F)
    val array = a match {
      case s: String => hexStringToByteArray(s) match {
        case Left(err) => throw new NumberFormatException(err + " The value was '%s'.".format(s))
        case Right(hb) => hb
      }
      case hb: Array[Byte] => hb
      case x => dstate.pstate.SDE("XSHexBinary received a value other than String or Array[Byte]. The value is class %s.", Misc.getNameFromClass(x))
    }
    array
  }
}

/**
 * The argument can
 * also be a long, unsignedLong, or any subtype
 * thereof, and in that case a xs:hexBinary value
 * containing a number of hex digits is produced.
 * The ordering and number of the digits
 * correspond to a binary big-endian twos-
 * complement implementation of the type of the
 * argument. Digits 0-9, A-F are used.
 * The number of digits produced depends on the
 * type of $arg, being 2, 4, 8 or 16. If $arg is a
 * literal number then the type is the smallest
 * signed type (long, int, short, byte) that can
 * contain the value.
 * If a literal number is not able to be represented
 * by a long, it is a schema definition error.
 *
 * • dfdl:hexBinary(xs:short(208)) is the hexBinary value "00D0".
 * • dfdl:hexBinary(208) is the hexBinary value "D0".
 * • dfdl:hexBinary(-2084) is the hexBinary value "F7FF".
 *
 */
case class DFDLHexBinary(recipe: DPathRecipe, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) with HexBinaryKind {
  val name = "DFDLHexBinary"
  private val conversionErrMsg: String = "%s could not be represented as a long."

  private def getNBytesFromArray(a: Array[Byte], n: Int): Array[Byte] = {
    val newArray = new Array[Byte](n)

    var i: Int = n
    val aLen = a.length
    val idxNewArray: Int = 0
    val offset: Int = if (aLen < n) { Math.abs(aLen - n) } else 0

    for (idxNewArray <- 0 + offset to n - 1) {
      newArray(idxNewArray) = a(aLen - i + offset)
      i -= 1
    }

    newArray
  }
  private def reduce(bi: BigInt): Either[String, Array[Byte]] = {

    val arr = bi.toByteArray

    val res: Either[String, Array[Byte]] = {
      if (bi.isValidByte) Right(getNBytesFromArray(arr, 1))
      else if (bi.isValidShort) Right(getNBytesFromArray(arr, 2))
      else if (bi.isValidInt) Right(getNBytesFromArray(arr, 4))
      else if (bi.isValidLong) Right(getNBytesFromArray(arr, 8))
      else Left(conversionErrMsg.format(bi.toString))
    }
    res
  }

  /**
   * Convo w/ Mike:
   *
   * (02:35:13 PM) Mike Beckerle: If the argument is of some fixed-width type like Byte, Int, Short, Long or the unsigned thereof, then you get hex digits corresponding to 1, 2, 4, or 8 bytes of a binary twos-complement integer value of that type. If the argument is anything else (including is a literal number), then you get the smallest number of hex digit pairs that can represent the value.  If you get xs:integer (aka BigInt/Java BigInteger), then I'd say - smallest number of digits that can represent the value.
   * (02:35:34 PM) Taylor: ok
   * (02:35:50 PM) Mike Beckerle: So dfdl:hexBinary(208) is D0, dfdl:hexBinary(xs:integer(208)) is also D0 dfdl:hexBinary(xs:short(208)) is 00D0
   * (02:36:13 PM) Taylor: ah ok
   * (02:36:56 PM) Taylor: I also think the resultant value for -2084 is incorrect
   * (02:37:25 PM) Taylor: Think it should be F7DC unless I'm doing something wrong.
   * (02:38:48 PM) Mike Beckerle: Probably just typo.
   * (02:39:40 PM) Mike Beckerle: F7DC is definitely right.
   * (02:41:27 PM) Taylor: Ok thanks.  So Integer is really going to be the exception here. It will always be the smallest value that can be represented vs the other types.
   * (02:42:23 PM) Taylor: It seems like regardless of whether or not I use xs:integer(208) or just 208 we get an Integer back.
   * (02:42:41 PM) Taylor: There's no BigInt/BigInteger distinction.
   * (02:44:17 PM) Mike Beckerle: Yeah I think scala does a BigInt conversion implicitly perhaps? They may be just type synonyms also. I'm not sure.
   * (02:45:03 PM) Mike Beckerle: The code that creates literal numeral constants does not downsize them. It should be producing xs:int, I believe, for all literal numbers that fit in that range.
   */
  override def computeValue(a: Any, dstate: DState): Any = {
    val arr = a match {
      case s: String => {
        // Literal number

        val result = reduce(BigInt(s)) match {
          case Left(err) => dstate.pstate.SDE(err + " The value was '%s'.", s)
          case Right(hb) => hb
        }
        result
      }
      case b: Byte => {
        val arr = new Array[Byte](1)
        arr(0) = b
        arr
      }
      case s: Short => {
        val hexString: String = "%04X".format(s)
        hexStringToByteArray(hexString) match {
          case Left(err) => dstate.pstate.SDE(err + " The value was '%s'.", s)
          case Right(hb) => hb
        }
      }
      case bi: BigInt => {
        // Literal number

        val result = reduce(bi) match {
          case Left(err) => dstate.pstate.SDE(err + " The value was '%s'.", bi.toString)
          case Right(hb) => hb
        }
        result
      }
      case i: Integer => {
        // Possibly a Literal Number, try to fit it into the smallest
        // value anyway.
        val result = reduce(BigInt(i)) match {
          case Left(err) => dstate.pstate.SDE(err + " The value was '%s'.", i)
          case Right(hb) => hb
        }
        result
      }
      case l: Long => {
        val hexString = "%016X".format(l)
        hexStringToByteArray(hexString) match {
          case Left(err) => dstate.pstate.SDE(err + " The value was '%s'.", l)
          case Right(hb) => hb
        }
      }
      case ul: BigDecimal => {
        val hexString = "%032X".format(ul)
        hexStringToByteArray(hexString) match {
          case Left(err) => dstate.pstate.SDE(err + " The value was '%s'.", ul)
          case Right(hb) => hb
        }
      }
      case hb: Array[Byte] => hb
      case x => dstate.pstate.SDE("Unrecognized type! Must be String or Array[Byte] The value was '%s'.", x)
    }

    arr
  }
}

case class NumericOperator(nop: NumericOp, left: DPathRecipe, right: DPathRecipe)
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

trait NumericOp extends AsIntMixin {
  /**
   * It is such a pain that there is no scala.math.Number base class above
   * all the numeric types.
   */
  def operate(v1: Any, v2: Any): Any
}

//case class XSConverter(sourceRecipe: DPathRecipe, sourceType: NodeInfo.Kind, targetType: NodeInfo.Kind)
//  extends RecipeOp {
//
//  override def toXML = <XSConverter source={ sourceType.toString } target={ targetType.toString }/>
//
//  override def run(dstate: DState) {
//    val savedNode = dstate.currentNode
//    sourceRecipe.run(dstate)
//    val sourceValue = dstate.currentValue
//    dstate.setCurrentNode(savedNode)
//
//    val targetValue = try {
//      val tv = Conversion.convertTo(sourceType, targetType, sourceValue)
//      tv
//    } catch {
//      case e: NumberFormatException => throw new NumberFormatException("Could not convert \"%s\" of type %s to %s: %s".format(sourceValue, sourceType, targetType, e.getMessage))
//    }
//
//    dstate.setCurrentValue(targetValue)
//  }
//}

case class DAFTrace(recipe: DPathRecipe, msg: String)
  extends FNOneArg(recipe, NodeInfo.AnyType) {

  override def computeValue(str: Any, dstate: DState) = {
    System.err.println("trace " + msg + ":" + str.toString)
    str
  }
}

abstract class Converter extends RecipeOp {

  final val maxUnsignedLong = BigInt("18446744073709551615")

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

//case object AnyAtomicToHexBinary extends Converter {
//  override def computeValue(a: Any, dstate: DState) = {
//    Console.out.println(a.getClass)
//    a match {
//      case hb: Array[Byte] => hb
//      case s: String => XSHexBinary.computeValue(s, dstate)
//      case _ => a.asInstanceOf[String]
//    }
//  }
//}

case object BooleanToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = if (asBoolean(a) == true) 1L else 0L
}

case object BooleanToString extends Converter {
  override def computeValue(a: Any, dstate: DState) = if (asBoolean(a) == true) "true" else "" // empty string is false in XPath
}

case object DateTimeToDate extends Converter {
  override def computeValue(a: Any, dstate: DState) = a.asInstanceOf[DFDLCalendar].toDate
}
case object DateTimeToTime extends Converter {
  override def computeValue(a: Any, dstate: DState) = a.asInstanceOf[DFDLCalendar].toTime
}
case object DateToDateTime extends Converter {
  override def computeValue(a: Any, dstate: DState) = a.asInstanceOf[DFDLCalendar].toDateTime
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
    if (res > this.maxUnsignedLong) throw new NumberFormatException("Value %s out of range for UnsignedLong type.".format(res))
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
    if (res > this.maxUnsignedLong) throw new NumberFormatException("Value %s out of range for UnsignedLong type.".format(res))
    else res
  }
}
case object FloatToDouble extends Converter {
  override def computeValue(a: Any, dstate: DState) = asFloat(a).toDouble
}
case object IntegerToDecimal extends Converter {
  override def computeValue(a: Any, dstate: DState) = BigDecimal(asBigInt(a))
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
    if (res > this.maxUnsignedLong) throw new NumberFormatException("Value %s out of range for UnsignedLong type.".format(res))
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

  def getNewCalendar: Calendar = calendar.get.clone().asInstanceOf[Calendar]
}

trait DateTimeFormatters {
  lazy val defaultFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSSSSxxxxx")
      format
    }
  }

  lazy val withFractNoTimeZoneFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSSSS")
      format
    }
  }

  lazy val withTimeZoneNoFractFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssxxxxx")
      format
    }
  }

  lazy val noTimeZoneNoFractFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")
      format
    }
  }

  lazy val withTimeZoneFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("yyyy-MM-ddxxxxx")
      format
    }
  }

  lazy val dateOnlyFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("yyyy-MM-dd")
      format
    }
  }
}

trait DateFormatters {
  lazy val defaultFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("yyyy-MM-ddxxxxx")
      format
    }
  }
  lazy val withoutTimezoneFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("yyyy-MM-dd")
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

case object XSDate extends Converter with XSDateTimeKind with DateFormatters {
  val name = "XSDate"

  override def computeValue(a: Any, dstate: DState): Any = {
    val defaultFormat = defaultFormatter.get
    defaultFormat.setCalendar(getNewCalendar)

    Conversion.stringToDFDLCalendar(a.toString, defaultFormat, defaultFormat, name, "Date") match {
      case Left(failure1) => {
        val formatWithoutTimezone = withoutTimezoneFormatter.get
        formatWithoutTimezone.setCalendar(getNewCalendar)

        Conversion.stringToDFDLCalendar(a.toString, formatWithoutTimezone, defaultFormat, name, "Date") match {
          case Left(failure2) => dstate.pstate.SDE("XSDate failed due to: " + failure1 + " and " + failure2)
          case Right(cal) => return cal
        }
      }
      case Right(cal) => return cal
    }
  }
}
case object XSDateTime extends Converter with XSDateTimeKind with DateTimeFormatters {
  val name = "XSDateTime"

  override def computeValue(a: Any, dstate: DState): Any = {
    val defaultFormat = defaultFormatter.get
    defaultFormat.setCalendar(getNewCalendar)
    Conversion.stringToDFDLCalendar(a.toString, defaultFormat, defaultFormat, name, "DateTime") match {
      case Left(failure1) => {
        val dateTimeWithFractNoTimeZone = withFractNoTimeZoneFormatter.get
        dateTimeWithFractNoTimeZone.setCalendar(getNewCalendar)
        Conversion.stringToDFDLCalendar(a.toString, dateTimeWithFractNoTimeZone, defaultFormat, name, "DateTime") match {
          case Left(failure2) => {
            val dateTimeWithTimeZoneNoFract = withTimeZoneNoFractFormatter.get
            dateTimeWithTimeZoneNoFract.setCalendar(getNewCalendar)
            Conversion.stringToDFDLCalendar(a.toString, dateTimeWithTimeZoneNoFract, defaultFormat, name, "DateTime") match {
              case Left(failure3) => {
                val dateTimeNoTimeZoneNoFract = noTimeZoneNoFractFormatter.get
                dateTimeNoTimeZoneNoFract.setCalendar(getNewCalendar)
                Conversion.stringToDFDLCalendar(a.toString, dateTimeNoTimeZoneNoFract, defaultFormat, name, "DateTime") match {
                  case Left(failure4) => {
                    val dateWithTimeZoneFormat = withTimeZoneFormatter.get
                    dateWithTimeZoneFormat.setCalendar(getNewCalendar)
                    Conversion.stringToDFDLCalendar(a.toString, dateWithTimeZoneFormat, defaultFormat, name, "DateTime") match {
                      case Left(failure5) => {
                        val dateOnlyFormat = dateOnlyFormatter.get
                        dateOnlyFormat.setCalendar(getNewCalendar)
                        Conversion.stringToDFDLCalendar(a.toString, dateOnlyFormat, defaultFormat, name, "DateTime") match {
                          case Left(failure6) => {
                            dstate.pstate.SDE("XSDate failed due to: " + failure1 + "\n" + failure2 +
                              "\n" + failure3 + "\n" + failure4 + "\n" + failure5 + "\n" + failure6)
                          }
                          case Right(cal) => return cal
                        }
                      }
                      case Right(cal) => return cal
                    }
                  }
                  case Right(cal) => return cal
                }
              }
              case Right(cal) => return cal
            }
          }
          case Right(cal) => return cal
        }
      }
      case Right(cal) => return cal
    }
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
      case s: String =>
        hexStringToByteArray(s) match {
          case Left(err) => throw new NumberFormatException(err + " The value was '%s'.".format(s))
          case Right(hb) => hb
        }
      case hb: Array[Byte] => hb
      case x => dstate.pstate.SDE("Unrecognized type! Must be String or Array[Byte] The value was '%s'.", x)
    }

    arr
  }
}

/**
 * The argument can
 * also be a long, unsignedLong, or any subtype
 * thereof, and in that case a xs:hexBinary value
 * containing a number of hex digits is produced.
 * The ordering and number of the digits
 * correspond to a binary big-endian twos-
 * complement implementation of the type of the
 * argument. Digits 0-9, A-F are used.
 * The number of digits produced depends on the
 * type of $arg, being 2, 4, 8 or 16. If $arg is a
 * literal number then the type is the smallest
 * signed type (long, int, short, byte) that can
 * contain the value.
 * If a literal number is not able to be represented
 * by a long, it is a schema definition error.
 */
case object DFDLHexBinary extends Converter with HexBinaryKind {
  val name = "DFDLHexBinary"

  override def computeValue(a: Any, dstate: DState): Any = {
    // Apparently an invariant at DPath.scala 156 wants this to be
    // Array[Byte]
    //
    // Check for:
    // 1. Even number of characters
    // 2. Valid hex (0-9 A-F)
    val arr = a match {
      case s: String =>
        hexStringToByteArray(s) match {
          case Left(err) => dstate.pstate.SDE(err + " The value was '%s'.", s)
          case Right(hb) => hb
        }
      case b: Byte => {
        val arr = new Array[Byte](1)
        arr(0) = b
        arr
      }
      case s: Short => {
        hexStringToByteArray(s.toInt.toHexString) match {
          case Left(err) => dstate.pstate.SDE(err + " The value was '%s'.", s)
          case Right(hb) => hb
        }
      }
      case i: Integer => {
        hexStringToByteArray(i.toInt.toHexString) match {
          case Left(err) => dstate.pstate.SDE(err + " The value was '%s'.", i)
          case Right(hb) => hb
        }
      }
      case l: Long => {
        hexStringToByteArray(l.toHexString) match {
          case Left(err) => dstate.pstate.SDE(err + " The value was '%s'.", l)
          case Right(hb) => hb
        }
      }
      case ul: BigDecimal => {
        hexStringToByteArray(ul.toBigInt.toString(16)) match {
          case Left(err) => dstate.pstate.SDE(err + " The value was '%s'.", ul)
          case Right(hb) => hb
        }
      }
      case hb: Array[Byte] => hb
      case x => dstate.pstate.SDE("Unrecognized type! Must be String or Array[Byte] The value was '%s'.", x)
    }

    arr
  }
}

case object XSTime extends Converter with XSDateTimeKind with TimeFormatters {
  val name = "XSTime"

  override def computeValue(a: Any, dstate: DState): Any = {
    val defaultFormat = defaultFormatter.get
    defaultFormat.setCalendar(getNewCalendar)
    Conversion.stringToDFDLCalendar(a.toString, defaultFormat, defaultFormat, name, "Time") match {
      case Left(failure1) => {
        val formatNoTimeZone = noTimeZoneFormatter.get
        formatNoTimeZone.setCalendar(getNewCalendar)
        Conversion.stringToDFDLCalendar(a.toString, formatNoTimeZone, defaultFormat, name, "Time") match {
          case Left(failure2) => {
            val formatNoTimeZoneNoFractional = noTimeZoneNoFractFormatter.get
            formatNoTimeZoneNoFractional.setCalendar(getNewCalendar)
            Conversion.stringToDFDLCalendar(a.toString, formatNoTimeZoneNoFractional, defaultFormat, name, "Time") match {
              case Left(failure3) => {
                val formatWithTimeZoneNoFractional = withTimeZoneNoFractFormatter.get
                formatWithTimeZoneNoFractional.setCalendar(getNewCalendar)
                Conversion.stringToDFDLCalendar(a.toString, formatWithTimeZoneNoFractional, defaultFormat, name, "Time") match {
                  case Left(failure4) => dstate.pstate.SDE("XSTime failed due to " + failure1 + ", " + failure2 + ", " + failure3 + " and " + failure4)
                  case Right(cal) => return cal
                }
              }
              case Right(cal) => return cal
            }
          }
          case Right(cal) => return cal
        }
      }
      case Right(cal) => return cal
    }
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

