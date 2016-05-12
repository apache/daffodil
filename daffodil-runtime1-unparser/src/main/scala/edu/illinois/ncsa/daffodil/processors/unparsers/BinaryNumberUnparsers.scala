package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.parsers.HasKnownLengthInBits
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.ParseOrUnparseState
import edu.illinois.ncsa.daffodil.util.Maybe._
import java.lang.{ Number => JNumber, Long => JLong }
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.parsers.HasRuntimeExplicitLength
import edu.illinois.ncsa.daffodil.processors.Evaluatable
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthUnits
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.YesNo
import edu.illinois.ncsa.daffodil.io.DataOutputStream
import edu.illinois.ncsa.daffodil.dpath.AsIntConverters._

abstract class BinaryNumberBaseUnparser(e: ElementRuntimeData)
  extends PrimUnparserObject(e) {

  protected def getBitLength(s: ParseOrUnparseState): Int

  protected def putNumber(dos: DataOutputStream, number: JNumber, nBits: Int): Boolean

  def unparse(state: UState): Unit = {
    val nBits = getBitLength(state)
    val node = state.currentInfosetNode.asSimple
    val value = node.dataValue.asInstanceOf[JNumber]
    val dos = state.dataOutputStream

    val res = putNumber(dos, value, nBits)

    if (!res) {
      Assert.invariant(dos.maybeRelBitLimit0b.isDefined)
      UnparseError(One(state.schemaFileLocation), One(state.currentLocation), "Insufficient space to unparse element %s, required %s bits, but only %s were available.",
        e.dpathElementCompileInfo.namedQName.toPrettyString, nBits, dos.maybeRelBitLimit0b.get)
    }
  }

}

abstract class BinaryIntegerBaseUnparser(e: ElementRuntimeData, signed: Boolean)
  extends BinaryNumberBaseUnparser(e) {

  override def putNumber(dos: DataOutputStream, value: JNumber, nBits: Int): Boolean = {
    if (nBits > 64) {
      dos.putBigInt(asBigInt(value), nBits)
    } else {
      dos.putLong(asLong(value), nBits)
    }
  }
}

class BinaryIntegerKnownLengthUnparser(e: ElementRuntimeData, signed: Boolean, override val lengthInBits: Int)
  extends BinaryIntegerBaseUnparser(e, signed)
  with HasKnownLengthInBits {
}

class BinaryIntegerRuntimeLengthUnparser(val e: ElementRuntimeData, signed: Boolean, val lengthEv: Evaluatable[JLong], val lUnits: LengthUnits)
  extends BinaryIntegerBaseUnparser(e, signed)
  with HasRuntimeExplicitLength {

  override val runtimeDependencies = List(lengthEv)
}

class BinaryFloatUnparser(e: ElementRuntimeData)
  extends BinaryNumberBaseUnparser(e) {

  override def getBitLength(s: ParseOrUnparseState) = 32

  override def putNumber(dos: DataOutputStream, value: JNumber, nBits: Int): Boolean = {
    dos.putBinaryFloat(asFloat(value))
  }

}

class BinaryDoubleUnparser(e: ElementRuntimeData)
  extends BinaryNumberBaseUnparser(e) {

  override def getBitLength(s: ParseOrUnparseState) = 64

  override def putNumber(dos: DataOutputStream, value: JNumber, nBits: Int): Boolean = {
    dos.putBinaryDouble(asDouble(value))
  }
}

class BinaryDecimalKnownLengthUnparser(e: ElementRuntimeData, signed: YesNo, binaryDecimalVirtualPoint: Int, val lengthInBits: Int)
  extends BinaryDecimalUnparserBase(e, signed, binaryDecimalVirtualPoint)
  with HasKnownLengthInBits {
}

class BinaryDecimalRuntimeLengthUnparser(val e: ElementRuntimeData, signed: YesNo, binaryDecimalVirtualPoint: Int, val lengthEv: Evaluatable[JLong], val lUnits: LengthUnits)
  extends BinaryDecimalUnparserBase(e, signed, binaryDecimalVirtualPoint)
  with HasRuntimeExplicitLength {

  override val runtimeDependencies = List(lengthEv)
}

abstract class BinaryDecimalUnparserBase(e: ElementRuntimeData, signed: YesNo, binaryDecimalVirtualPoint: Int)
  extends BinaryNumberBaseUnparser(e) {

  private val tenBigDec = BigDecimal(10)

  override def putNumber(dos: DataOutputStream, value: JNumber, nBits: Int): Boolean = {

    // We want to scale the bigInt by binaryDecimalVirtualPoint so that it is a BigInt
    val bigDec = asBigDecimal(value)
    val bigInt =
      if (binaryDecimalVirtualPoint != 0) (bigDec * tenBigDec.pow(binaryDecimalVirtualPoint)).toBigInt
      else bigDec.toBigInt

    dos.putBigInt(bigInt, nBits)

  }
}
