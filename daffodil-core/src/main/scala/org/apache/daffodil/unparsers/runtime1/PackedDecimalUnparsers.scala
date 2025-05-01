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

package org.apache.daffodil.unparsers.runtime1

import java.lang.{ Long => JLong }
import java.math.{ BigInteger => JBigInteger }

import org.apache.daffodil.lib.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.lib.util.DecimalUtils
import org.apache.daffodil.lib.util.PackedSignCodes
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Evaluatable
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.parsers.HasKnownLengthInBits
import org.apache.daffodil.runtime1.processors.parsers.HasRuntimeExplicitLength
import org.apache.daffodil.runtime1.processors.unparsers._

abstract class PackedIntegerBaseUnparser(
  e: ElementRuntimeData,
  packedSignCodes: PackedSignCodes
) extends PackedBinaryIntegerBaseUnparser(e) {

  override def fromBigInteger(bigInt: JBigInteger, nBits: Int): Array[Byte] =
    DecimalUtils.packedFromBigInteger(bigInt, nBits, packedSignCodes)
}

class PackedIntegerKnownLengthUnparser(
  e: ElementRuntimeData,
  packedSignCodes: PackedSignCodes,
  override val lengthInBits: Int
) extends PackedIntegerBaseUnparser(e, packedSignCodes)
  with HasKnownLengthInBits {}

class PackedIntegerRuntimeLengthUnparser(
  val e: ElementRuntimeData,
  packedSignCodes: PackedSignCodes,
  val lengthEv: Evaluatable[JLong],
  val lengthUnits: LengthUnits
) extends PackedIntegerBaseUnparser(e, packedSignCodes)
  with HasRuntimeExplicitLength {

  override def runtimeDependencies = Vector(lengthEv)
}

final class PackedIntegerDelimitedUnparser(
  e: ElementRuntimeData,
  packedSignCodes: PackedSignCodes
) extends PackedIntegerBaseUnparser(e, packedSignCodes) {

  override def getBitLength(state: ParseOrUnparseState): Int = { 0 }
}

final class PackedIntegerMinimumLengthUnparser(
  e: ElementRuntimeData,
  packedSignCodes: PackedSignCodes
) extends PackedIntegerBaseUnparser(e, packedSignCodes) {

  override def runtimeDependencies = Vector()

  override def getBitLength(s: ParseOrUnparseState): Int = {
    val number = getNumberToPut(s.asInstanceOf[UState])
    val absBigIntStr = number.asInstanceOf[JBigInteger].abs.toString
    val (byteLength, _) = DecimalUtils.packedFromBigIntegerLength(absBigIntStr, 0)
    byteLength * 8
  }
}

abstract class PackedDecimalBaseUnparser(
  e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  packedSignCodes: PackedSignCodes
) extends PackedBinaryDecimalBaseUnparser(e, binaryDecimalVirtualPoint) {

  override def fromBigInteger(bigInt: JBigInteger, nBits: Int): Array[Byte] =
    DecimalUtils.packedFromBigInteger(bigInt, nBits, packedSignCodes)
}

class PackedDecimalKnownLengthUnparser(
  e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  packedSignCodes: PackedSignCodes,
  override val lengthInBits: Int
) extends PackedDecimalBaseUnparser(e, binaryDecimalVirtualPoint, packedSignCodes)
  with HasKnownLengthInBits {}

class PackedDecimalRuntimeLengthUnparser(
  val e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  packedSignCodes: PackedSignCodes,
  val lengthEv: Evaluatable[JLong],
  val lengthUnits: LengthUnits
) extends PackedDecimalBaseUnparser(e, binaryDecimalVirtualPoint, packedSignCodes)
  with HasRuntimeExplicitLength {

  override def runtimeDependencies = Vector(lengthEv)
}

final class PackedDecimalDelimitedUnparser(
  e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  packedSignCodes: PackedSignCodes
) extends PackedDecimalBaseUnparser(e, binaryDecimalVirtualPoint, packedSignCodes) {

  override def getBitLength(state: ParseOrUnparseState): Int = { 0 }
}

final class PackedDecimalMinimumLengthUnparser(
  e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  packedSignCodes: PackedSignCodes
) extends PackedDecimalBaseUnparser(e, binaryDecimalVirtualPoint, packedSignCodes) {

  override def runtimeDependencies = Vector()

  override def getBitLength(s: ParseOrUnparseState): Int = {
    val number = getNumberToPut(s.asInstanceOf[UState])
    val absBigIntStr = number.asInstanceOf[JBigInteger].abs.toString
    val (byteLength, _) = DecimalUtils.packedFromBigIntegerLength(absBigIntStr, 0)
    byteLength * 8
  }

}
