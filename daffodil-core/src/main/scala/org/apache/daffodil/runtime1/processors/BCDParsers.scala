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
import org.apache.daffodil.lib.util.DecimalUtils
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Evaluatable

class BCDDecimalKnownLengthParser(
  e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  val lengthInBits: Int
) extends PackedBinaryDecimalBaseParser(e, binaryDecimalVirtualPoint)
  with HasKnownLengthInBits {

  override def toNumber(num: Array[Byte]): JBigDecimal =
    DecimalUtils.bcdToBigDecimal(num, binaryDecimalVirtualPoint)

}

class BCDDecimalRuntimeLengthParser(
  val e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  val lengthEv: Evaluatable[JLong],
  val lengthUnits: LengthUnits
) extends PackedBinaryDecimalBaseParser(e, binaryDecimalVirtualPoint)
  with HasRuntimeExplicitLength {

  override def toNumber(num: Array[Byte]): JBigDecimal =
    DecimalUtils.bcdToBigDecimal(num, binaryDecimalVirtualPoint)

}

class BCDDecimalBitLimitLengthParser(
  e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int
) extends PackedBinaryDecimalBaseParser(e, binaryDecimalVirtualPoint)
  with BitLengthFromBitLimitMixin {

  override def toNumber(num: Array[Byte]): JBigDecimal =
    DecimalUtils.bcdToBigDecimal(num, binaryDecimalVirtualPoint)
}

class BCDIntegerRuntimeLengthParser(
  val e: ElementRuntimeData,
  val lengthEv: Evaluatable[JLong],
  val lengthUnits: LengthUnits
) extends PackedBinaryIntegerBaseParser(e)
  with HasRuntimeExplicitLength {

  override def toNumber(num: Array[Byte]): JBigInteger = DecimalUtils.bcdToBigInteger(num)

}

class BCDIntegerKnownLengthParser(e: ElementRuntimeData, val lengthInBits: Int)
  extends PackedBinaryIntegerBaseParser(e)
  with HasKnownLengthInBits {

  override def toNumber(num: Array[Byte]): JBigInteger = DecimalUtils.bcdToBigInteger(num)

}

class BCDIntegerBitLimitLengthParser(e: ElementRuntimeData)
  extends PackedBinaryIntegerBaseParser(e)
  with BitLengthFromBitLimitMixin {

  override def toNumber(num: Array[Byte]): JBigInteger = DecimalUtils.bcdToBigInteger(num)
}
