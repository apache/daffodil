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

package org.apache.daffodil.runtime1.processors.parsers

import java.lang.{ Long => JLong }
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInteger }

import org.apache.daffodil.lib.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.lib.util.{ DecimalUtils, PackedSignCodes }
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Evaluatable
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.Processor

class PackedDecimalKnownLengthParser(
  e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  packedSignCodes: PackedSignCodes,
  val lengthInBits: Int
) extends PackedBinaryDecimalBaseParser(e, binaryDecimalVirtualPoint)
  with HasKnownLengthInBits {

  override def toNumber(num: Array[Byte]): JBigDecimal =
    DecimalUtils.packedToBigDecimal(num, binaryDecimalVirtualPoint, packedSignCodes)

}

class PackedDecimalRuntimeLengthParser(
  val e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  packedSignCodes: PackedSignCodes,
  val lengthEv: Evaluatable[JLong],
  val lengthUnits: LengthUnits
) extends PackedBinaryDecimalBaseParser(e, binaryDecimalVirtualPoint)
  with HasRuntimeExplicitLength {

  override def toNumber(num: Array[Byte]): JBigDecimal =
    DecimalUtils.packedToBigDecimal(num, binaryDecimalVirtualPoint, packedSignCodes)
}

class PackedDecimalPrefixedLengthParser(
  e: ElementRuntimeData,
  override val prefixedLengthParser: Parser,
  override val prefixedLengthERD: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  packedSignCodes: PackedSignCodes,
  override val lengthUnits: LengthUnits,
  override val prefixedLengthAdjustmentInUnits: Long
) extends PackedBinaryDecimalBaseParser(e, binaryDecimalVirtualPoint)
  with PrefixedLengthParserMixin {

  override def toNumber(num: Array[Byte]): JBigDecimal =
    DecimalUtils.packedToBigDecimal(num, binaryDecimalVirtualPoint, packedSignCodes)

  override def childProcessors: Vector[Processor] = Vector(prefixedLengthParser)

  override def getBitLength(state: ParseOrUnparseState): Int = {
    getPrefixedLengthInBits(state.asInstanceOf[PState]).toInt
  }

}

class PackedIntegerRuntimeLengthParser(
  val e: ElementRuntimeData,
  packedSignCodes: PackedSignCodes,
  val lengthEv: Evaluatable[JLong],
  val lengthUnits: LengthUnits
) extends PackedBinaryIntegerBaseParser(e)
  with HasRuntimeExplicitLength {

  override def toNumber(num: Array[Byte]): JBigInteger =
    DecimalUtils.packedToBigInteger(num, packedSignCodes)

}

class PackedIntegerKnownLengthParser(
  e: ElementRuntimeData,
  packedSignCodes: PackedSignCodes,
  val lengthInBits: Int
) extends PackedBinaryIntegerBaseParser(e)
  with HasKnownLengthInBits {

  override def toNumber(num: Array[Byte]): JBigInteger =
    DecimalUtils.packedToBigInteger(num, packedSignCodes)

}

class PackedIntegerPrefixedLengthParser(
  e: ElementRuntimeData,
  override val prefixedLengthParser: Parser,
  override val prefixedLengthERD: ElementRuntimeData,
  packedSignCodes: PackedSignCodes,
  override val lengthUnits: LengthUnits,
  override val prefixedLengthAdjustmentInUnits: Long
) extends PackedBinaryIntegerBaseParser(e)
  with PrefixedLengthParserMixin {

  override def toNumber(num: Array[Byte]): JBigInteger =
    DecimalUtils.packedToBigInteger(num, packedSignCodes)

  override def childProcessors: Vector[Processor] = Vector(prefixedLengthParser)

  override def getBitLength(state: ParseOrUnparseState): Int = {
    getPrefixedLengthInBits(state.asInstanceOf[PState]).toInt
  }
}
