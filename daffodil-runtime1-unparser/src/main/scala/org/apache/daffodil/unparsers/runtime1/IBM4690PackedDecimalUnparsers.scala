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
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Evaluatable
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.parsers.HasKnownLengthInBits
import org.apache.daffodil.runtime1.processors.parsers.HasRuntimeExplicitLength
import org.apache.daffodil.runtime1.processors.unparsers._

abstract class IBM4690PackedIntegerBaseUnparser(e: ElementRuntimeData)
  extends PackedBinaryIntegerBaseUnparser(e) {

  override def fromBigInteger(bigInt: JBigInteger, nBits: Int): Array[Byte] =
    DecimalUtils.ibm4690FromBigInteger(bigInt, nBits)
}

class IBM4690PackedIntegerKnownLengthUnparser(
  e: ElementRuntimeData,
  override val lengthInBits: Int
) extends IBM4690PackedIntegerBaseUnparser(e)
  with HasKnownLengthInBits {}

class IBM4690PackedIntegerRuntimeLengthUnparser(
  val e: ElementRuntimeData,
  val lengthEv: Evaluatable[JLong],
  val lengthUnits: LengthUnits
) extends IBM4690PackedIntegerBaseUnparser(e)
  with HasRuntimeExplicitLength {

  override def runtimeDependencies = Vector(lengthEv)
}

final class IBM4690PackedIntegerDelimitedUnparser(e: ElementRuntimeData)
  extends IBM4690PackedIntegerBaseUnparser(e) {

  override def getBitLength(state: ParseOrUnparseState): Int = { 0 }
}

final class IBM4690PackedIntegerMinimumLengthUnparser(e: ElementRuntimeData)
  extends IBM4690PackedIntegerBaseUnparser(e) {

  override def runtimeDependencies = Vector()

  override def getBitLength(s: ParseOrUnparseState): Int = {
    val number = getNumberToPut(s.asInstanceOf[UState])
    val bigInt = number.asInstanceOf[JBigInteger]
    val absBigIntStr = bigInt.abs.toString
    val negative = (bigInt.signum != 1)
    val (byteLength, _) = DecimalUtils.ibm4690FromBigIntegerLength(absBigIntStr, 0, negative)
    byteLength * 8
  }
}

abstract class IBM4690PackedDecimalBaseUnparser(
  e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int
) extends PackedBinaryDecimalBaseUnparser(e, binaryDecimalVirtualPoint) {

  override def fromBigInteger(bigInt: JBigInteger, nBits: Int): Array[Byte] =
    DecimalUtils.ibm4690FromBigInteger(bigInt, nBits)
}

class IBM4690PackedDecimalKnownLengthUnparser(
  e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  override val lengthInBits: Int
) extends IBM4690PackedDecimalBaseUnparser(e, binaryDecimalVirtualPoint)
  with HasKnownLengthInBits {}

class IBM4690PackedDecimalRuntimeLengthUnparser(
  val e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  val lengthEv: Evaluatable[JLong],
  val lengthUnits: LengthUnits
) extends IBM4690PackedDecimalBaseUnparser(e, binaryDecimalVirtualPoint)
  with HasRuntimeExplicitLength {

  override def runtimeDependencies = Vector(lengthEv)
}

final class IBM4690PackedDecimalDelimitedUnparser(
  e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int
) extends IBM4690PackedDecimalBaseUnparser(e, binaryDecimalVirtualPoint) {

  override def getBitLength(state: ParseOrUnparseState): Int = { 0 }
}

final class IBM4690PackedDecimalMinimumLengthUnparser(
  e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int
) extends IBM4690PackedDecimalBaseUnparser(e, binaryDecimalVirtualPoint) {

  override def runtimeDependencies = Vector()

  override def getBitLength(s: ParseOrUnparseState): Int = {
    val number = getNumberToPut(s.asInstanceOf[UState])
    val bigInt = number.asInstanceOf[JBigInteger]
    val absBigIntStr = bigInt.abs.toString
    val negative = (bigInt.signum != 1)
    val (byteLength, _) = DecimalUtils.ibm4690FromBigIntegerLength(absBigIntStr, 0, negative)
    byteLength * 8
  }
}
