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

package org.apache.daffodil.processors.parsers

import java.lang.{ Long => JLong, Number => JNumber, Double => JDouble, Float => JFloat }
import java.math.{BigInteger => JBigInt, BigDecimal => JBigDecimal}

import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.dpath.InvalidPrimitiveDataException
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.processors.Processor
import org.apache.daffodil.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.schema.annotation.props.gen.YesNo

class BinaryFloatParser(override val context: ElementRuntimeData)
  extends PrimParser {
  override lazy val runtimeDependencies = Vector()

  def parse(start: PState): Unit = {
    val dis = start.dataInputStream

    if (!dis.isDefinedForLength(32)) {
      PENotEnoughBits(start, 32, dis.remainingBits)
      return
    }

    val f: JFloat = dis.getBinaryFloat(start)
    start.simpleElement.overwriteDataValue(f)
  }
}

class BinaryDoubleParser(override val context: ElementRuntimeData)
  extends PrimParser {
  override lazy val runtimeDependencies = Vector()

  def parse(start: PState): Unit = {
    val dis = start.dataInputStream

    if (!dis.isDefinedForLength(64)) {
      PENotEnoughBits(start, 64, dis.remainingBits)
      return
    }

    val d: JDouble = dis.getBinaryDouble(start)
    start.simpleElement.overwriteDataValue(d)
  }
}

class BinaryDecimalKnownLengthParser(e: ElementRuntimeData, signed: YesNo, binaryDecimalVirtualPoint: Int, val lengthInBits: Int)
  extends BinaryDecimalParserBase(e, signed, binaryDecimalVirtualPoint)
  with HasKnownLengthInBits {
}

class BinaryDecimalRuntimeLengthParser(val e: ElementRuntimeData, signed: YesNo, binaryDecimalVirtualPoint: Int, val lengthEv: Evaluatable[JLong], val lengthUnits: LengthUnits)
  extends BinaryDecimalParserBase(e, signed, binaryDecimalVirtualPoint)
  with HasRuntimeExplicitLength {
}

class BinaryDecimalPrefixedLengthParser(
  e: ElementRuntimeData,
  override val prefixedLengthParser: Parser,
  override val prefixedLengthERD: ElementRuntimeData,
  signed: YesNo,
  binaryDecimalVirtualPoint: Int,
  override val lengthUnits: LengthUnits,
  override val prefixedLengthAdjustmentInUnits: Long)
  extends BinaryDecimalParserBase(e, signed, binaryDecimalVirtualPoint)
  with PrefixedLengthParserMixin {

  override def childProcessors: Vector[Processor] = Vector(prefixedLengthParser)
      
  override def getBitLength(state: ParseOrUnparseState): Int = {
    getPrefixedLengthInBits(state.asInstanceOf[PState]).toInt
  }
}


abstract class BinaryDecimalParserBase(override val context: ElementRuntimeData, signed: YesNo, binaryDecimalVirtualPoint: Int)
  extends PrimParser {
  override lazy val runtimeDependencies = Vector()

  protected def getBitLength(s: ParseOrUnparseState): Int

  def parse(start: PState): Unit = {
    val nBits = getBitLength(start)
    val dis = start.dataInputStream
    if (!dis.isDefinedForLength(nBits)) {
      PENotEnoughBits(start, nBits, dis.remainingBits)
      return
    }

    val bigInt: JBigInt =
      if (signed == YesNo.Yes) { dis.getSignedBigInt(nBits, start) }
      else { dis.getUnsignedBigInt(nBits, start) }

    val bigDec = new JBigDecimal(bigInt, binaryDecimalVirtualPoint)
    start.simpleElement.overwriteDataValue(bigDec)
  }
}

class BinaryIntegerRuntimeLengthParser(val e: ElementRuntimeData, signed: Boolean, val lengthEv: Evaluatable[JLong], val lengthUnits: LengthUnits)
  extends BinaryIntegerBaseParser(e, signed)
  with HasRuntimeExplicitLength {
}

class BinaryIntegerKnownLengthParser(e: ElementRuntimeData, signed: Boolean, val lengthInBits: Int)
  extends BinaryIntegerBaseParser(e, signed)
  with HasKnownLengthInBits {
}

class BinaryIntegerPrefixedLengthParser(
  e: ElementRuntimeData,
  override val prefixedLengthParser: Parser,
  override val prefixedLengthERD: ElementRuntimeData,
  signed: Boolean,
  override val lengthUnits: LengthUnits,
  override val prefixedLengthAdjustmentInUnits: Long)
  extends BinaryIntegerBaseParser(e, signed)
  with PrefixedLengthParserMixin {

  override def childProcessors: Vector[Processor] = Vector(prefixedLengthParser)
      
  override def getBitLength(state: ParseOrUnparseState): Int = {
    getPrefixedLengthInBits(state.asInstanceOf[PState]).toInt
  }
}


abstract class BinaryIntegerBaseParser(override val context: ElementRuntimeData, signed: Boolean)
  extends PrimParser {
  override lazy val runtimeDependencies = Vector()

  protected def getBitLength(s: ParseOrUnparseState): Int

  private val primNumeric = context.optPrimType.get.asInstanceOf[NodeInfo.PrimType.PrimNumeric]

  def parse(start: PState): Unit = {
    val nBits = getBitLength(start)
    if (nBits == 0) return // zero length is used for outputValueCalc often.
    val dis = start.dataInputStream
    if (!dis.isDefinedForLength(nBits)) {
      PENotEnoughBits(start, nBits, dis.remainingBits)
      return
    }

    val num: JNumber =
      if (signed) {
        if (nBits > 64) { dis.getSignedBigInt(nBits, start) }
        else { dis.getSignedLong(nBits, start) }
      } else {
        if (nBits >= 64) { dis.getUnsignedBigInt(nBits, start) }
        else { dis.getUnsignedLong(nBits, start).toLong }
      }

    val res = try {
      primNumeric.fromNumber(num)
    } catch {
      case e: InvalidPrimitiveDataException => {
        PE(start, "%s", e.getMessage)
        return
      }
    }

    start.simpleElement.overwriteDataValue(res)
  }
}
