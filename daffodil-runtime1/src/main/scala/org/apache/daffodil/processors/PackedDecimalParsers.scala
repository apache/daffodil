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

import java.lang.{ Long => JLong }
import java.math.{ BigInteger => JBigInteger, BigDecimal => JBigDecimal }

import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.processors.Processor
import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.util.{ DecimalUtils, PackedSignCodes }

class PackedDecimalKnownLengthParser(
  e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  packedSignCodes: PackedSignCodes,
  val lengthInBits: Int)
  extends PackedBinaryDecimalBaseParser(e, binaryDecimalVirtualPoint)
  with HasKnownLengthInBits {

  override def toBigInteger(num: Array[Byte]): JBigInteger = DecimalUtils.packedToBigInteger(num, packedSignCodes)
  override def toBigDecimal(num: Array[Byte], scale: Int): JBigDecimal = DecimalUtils.packedToBigDecimal(num, scale, packedSignCodes)

}

class PackedDecimalRuntimeLengthParser(
  val e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  packedSignCodes: PackedSignCodes,
  val lengthEv: Evaluatable[JLong],
  val lengthUnits: LengthUnits)
  extends PackedBinaryDecimalBaseParser(e, binaryDecimalVirtualPoint)
  with HasRuntimeExplicitLength {

  override def toBigInteger(num: Array[Byte]): JBigInteger = DecimalUtils.packedToBigInteger(num, packedSignCodes)
  override def toBigDecimal(num: Array[Byte], scale: Int): JBigDecimal = DecimalUtils.packedToBigDecimal(num, scale, packedSignCodes)

}

class PackedDecimalPrefixedLengthParser(
  e: ElementRuntimeData,
  override val prefixedLengthParser: Parser,
  override val prefixedLengthERD: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  packedSignCodes: PackedSignCodes,
  override val lengthUnits: LengthUnits,
  override val prefixedLengthAdjustmentInUnits: Long)
  extends PackedBinaryDecimalBaseParser(e, binaryDecimalVirtualPoint)
  with PrefixedLengthParserMixin {

  override def toBigInteger(num: Array[Byte]): JBigInteger = DecimalUtils.packedToBigInteger(num, packedSignCodes)
  override def toBigDecimal(num: Array[Byte], scale: Int): JBigDecimal = DecimalUtils.packedToBigDecimal(num, scale, packedSignCodes)
  
  override def childProcessors: Vector[Processor] = Vector(prefixedLengthParser)
      
  override def getBitLength(state: ParseOrUnparseState): Int = {
    getPrefixedLengthInBits(state.asInstanceOf[PState]).toInt
  }

}


class PackedIntegerRuntimeLengthParser(
  val e: ElementRuntimeData,
  signed: Boolean,
  packedSignCodes: PackedSignCodes,
  val lengthEv: Evaluatable[JLong],
  val lengthUnits: LengthUnits)
  extends PackedBinaryIntegerBaseParser(e, signed)
  with HasRuntimeExplicitLength {

  override def toBigInteger(num: Array[Byte]): JBigInteger = DecimalUtils.packedToBigInteger(num, packedSignCodes)
  override def toBigDecimal(num: Array[Byte], scale: Int): JBigDecimal = DecimalUtils.packedToBigDecimal(num, scale, packedSignCodes)

}

class PackedIntegerKnownLengthParser(
  e: ElementRuntimeData,
  signed: Boolean,
  packedSignCodes: PackedSignCodes,
  val lengthInBits: Int)
  extends PackedBinaryIntegerBaseParser(e, signed)
  with HasKnownLengthInBits {

  override def toBigInteger(num: Array[Byte]): JBigInteger = DecimalUtils.packedToBigInteger(num, packedSignCodes)
  override def toBigDecimal(num: Array[Byte], scale: Int): JBigDecimal = DecimalUtils.packedToBigDecimal(num, scale, packedSignCodes)

}

class PackedIntegerPrefixedLengthParser(
  e: ElementRuntimeData,
  override val prefixedLengthParser: Parser,
  override val prefixedLengthERD: ElementRuntimeData,
  signed: Boolean,
  packedSignCodes: PackedSignCodes,
  override val lengthUnits: LengthUnits,
  override val prefixedLengthAdjustmentInUnits: Long)
  extends PackedBinaryIntegerBaseParser(e, signed)
  with PrefixedLengthParserMixin {

  override def toBigInteger(num: Array[Byte]): JBigInteger = DecimalUtils.packedToBigInteger(num, packedSignCodes)
  override def toBigDecimal(num: Array[Byte], scale: Int): JBigDecimal = DecimalUtils.packedToBigDecimal(num, scale, packedSignCodes)
  
  override def childProcessors: Vector[Processor] = Vector(prefixedLengthParser)
      
  override def getBitLength(state: ParseOrUnparseState): Int = {
    getPrefixedLengthInBits(state.asInstanceOf[PState]).toInt
  }
}

