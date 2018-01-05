/*
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors.Evaluatable
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthUnits
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.util.DecimalUtils
import java.lang.{ Long => JLong }
import java.math.{ BigInteger => JBigInteger, BigDecimal => JBigDecimal }

class BCDDecimalKnownLengthParser(
  e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  val lengthInBits: Int)
  extends PackedBinaryDecimalBaseParser(e, binaryDecimalVirtualPoint)
  with HasKnownLengthInBits {

  override def toBigInteger(num: Array[Byte]): JBigInteger = DecimalUtils.bcdToBigInteger(num)
  override def toBigDecimal(num: Array[Byte], scale: Int): JBigDecimal = DecimalUtils.bcdToBigDecimal(num, scale)

}

class BCDDecimalRuntimeLengthParser(
  val e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  val lengthEv: Evaluatable[JLong],
  val lUnits: LengthUnits)
  extends PackedBinaryDecimalBaseParser(e, binaryDecimalVirtualPoint)
  with HasRuntimeExplicitLength {

  override def toBigInteger(num: Array[Byte]): JBigInteger = DecimalUtils.bcdToBigInteger(num)
  override def toBigDecimal(num: Array[Byte], scale: Int): JBigDecimal = DecimalUtils.bcdToBigDecimal(num, scale)

}

class BCDIntegerRuntimeLengthParser(
  val e: ElementRuntimeData,
  val lengthEv: Evaluatable[JLong],
  val lUnits: LengthUnits)
  extends PackedBinaryIntegerBaseParser(e)
  with HasRuntimeExplicitLength {

  override def toBigInteger(num: Array[Byte]): JBigInteger = DecimalUtils.bcdToBigInteger(num)
  override def toBigDecimal(num: Array[Byte], scale: Int): JBigDecimal = DecimalUtils.bcdToBigDecimal(num, scale)

}

class BCDIntegerKnownLengthParser(
  e: ElementRuntimeData,
  val lengthInBits: Int)
  extends PackedBinaryIntegerBaseParser(e)
  with HasKnownLengthInBits {

  override def toBigInteger(num: Array[Byte]): JBigInteger = DecimalUtils.bcdToBigInteger(num)
  override def toBigDecimal(num: Array[Byte], scale: Int): JBigDecimal = DecimalUtils.bcdToBigDecimal(num, scale)

}
