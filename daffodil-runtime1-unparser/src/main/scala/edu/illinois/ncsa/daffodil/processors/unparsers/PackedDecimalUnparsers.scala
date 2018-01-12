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

package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.parsers.HasKnownLengthInBits
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.ParseOrUnparseState
import edu.illinois.ncsa.daffodil.util.{ DecimalUtils, PackedSignCodes }
import java.lang.{ Long => JLong }
import java.math.{ BigInteger => JBigInteger }
import edu.illinois.ncsa.daffodil.processors.parsers.HasRuntimeExplicitLength
import edu.illinois.ncsa.daffodil.processors.Evaluatable
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthUnits

abstract class PackedIntegerBaseUnparser(
  e: ElementRuntimeData,
  packedSignCodes: PackedSignCodes)
  extends PackedBinaryIntegerBaseUnparser(e) {

  override def fromBigInteger(bigInt: JBigInteger, nBits: Int): Array[Byte] = DecimalUtils.packedFromBigInteger(bigInt, nBits, packedSignCodes)
}

class PackedIntegerKnownLengthUnparser(
  e: ElementRuntimeData,
  packedSignCodes: PackedSignCodes,
  override val lengthInBits: Int)
  extends PackedIntegerBaseUnparser(e, packedSignCodes)
  with HasKnownLengthInBits {
}

class PackedIntegerRuntimeLengthUnparser(
  val e: ElementRuntimeData,
  packedSignCodes: PackedSignCodes,
  val lengthEv: Evaluatable[JLong],
  val lUnits: LengthUnits)
  extends PackedIntegerBaseUnparser(e, packedSignCodes)
  with HasRuntimeExplicitLength {

  override lazy val runtimeDependencies = List(lengthEv)
}

final class PackedIntegerMinLengthInBytesUnparser(
  minLengthInBytes: Int,
  e: ElementRuntimeData,
  packedSignCodes: PackedSignCodes)
  extends PackedIntegerBaseUnparser(e, packedSignCodes) {

  override def getBitLength(state: ParseOrUnparseState): Int = {
    val len = state.currentNode.get.asSimple.dataValue.asInstanceOf[Array[Byte]].length * 8
    val min = minLengthInBytes *  8
    scala.math.max(len, min)
  }
}


abstract class PackedDecimalBaseUnparser(
  e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  packedSignCodes: PackedSignCodes)
  extends PackedBinaryDecimalBaseUnparser(e, binaryDecimalVirtualPoint) {

  override def fromBigInteger(bigInt: JBigInteger, nBits: Int): Array[Byte] = DecimalUtils.packedFromBigInteger(bigInt, nBits, packedSignCodes)
}

class PackedDecimalKnownLengthUnparser(
  e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  packedSignCodes: PackedSignCodes,
  override val lengthInBits: Int)
  extends PackedDecimalBaseUnparser(e, binaryDecimalVirtualPoint, packedSignCodes)
  with HasKnownLengthInBits {
}

class PackedDecimalRuntimeLengthUnparser(
  val e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  packedSignCodes: PackedSignCodes,
  val lengthEv: Evaluatable[JLong],
  val lUnits: LengthUnits)
  extends PackedDecimalBaseUnparser(e, binaryDecimalVirtualPoint, packedSignCodes)
  with HasRuntimeExplicitLength {

  override lazy val runtimeDependencies = List(lengthEv)
}

final class PackedDecimalMinLengthInBytesUnparser(
  minLengthInBytes: Int,
  e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  packedSignCodes: PackedSignCodes)
  extends PackedDecimalBaseUnparser(e, binaryDecimalVirtualPoint, packedSignCodes) {

  override def getBitLength(state: ParseOrUnparseState): Int = {
    val len = state.currentNode.get.asSimple.dataValue.asInstanceOf[Array[Byte]].length * 8
    val min = minLengthInBytes *  8
    scala.math.max(len, min)
  }
}
