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

package org.apache.daffodil.grammar.primitives

import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.processors.parsers.BinaryDecimalKnownLengthParser
import org.apache.daffodil.processors.parsers.BinaryDecimalPrefixedLengthParser
import org.apache.daffodil.processors.parsers.BinaryDecimalRuntimeLengthParser
import org.apache.daffodil.processors.parsers.BinaryDoubleParser
import org.apache.daffodil.processors.parsers.BinaryFloatParser
import org.apache.daffodil.processors.parsers.BinaryIntegerKnownLengthParser
import org.apache.daffodil.processors.parsers.BinaryIntegerPrefixedLengthParser
import org.apache.daffodil.processors.parsers.BinaryIntegerRuntimeLengthParser
import org.apache.daffodil.processors.unparsers.BinaryDecimalKnownLengthUnparser
import org.apache.daffodil.processors.unparsers.BinaryDecimalPrefixedLengthUnparser
import org.apache.daffodil.processors.unparsers.BinaryDecimalRuntimeLengthUnparser
import org.apache.daffodil.processors.unparsers.BinaryDoubleUnparser
import org.apache.daffodil.processors.unparsers.BinaryFloatUnparser
import org.apache.daffodil.processors.unparsers.BinaryIntegerKnownLengthUnparser
import org.apache.daffodil.processors.unparsers.BinaryIntegerRuntimeLengthUnparser
import org.apache.daffodil.processors.unparsers.BinaryIntegerPrefixedLengthUnparser
import org.apache.daffodil.processors.unparsers.Unparser
import org.apache.daffodil.util.MaybeInt

class BinaryIntegerRuntimeLength(val e: ElementBase, signed: Boolean) extends Terminal(e, true) {

  override lazy val parser = new BinaryIntegerRuntimeLengthParser(e.elementRuntimeData, signed, e.lengthEv, e.lengthUnits)

  override lazy val unparser: Unparser = new BinaryIntegerRuntimeLengthUnparser(e.elementRuntimeData, signed, e.lengthEv, e.lengthUnits)
}

class BinaryIntegerKnownLength(val e: ElementBase, signed: Boolean, lengthInBits: Long) extends Terminal(e, true) {

  override lazy val parser = {
    new BinaryIntegerKnownLengthParser(e.elementRuntimeData, signed, lengthInBits.toInt)
  }

  override lazy val unparser: Unparser = new BinaryIntegerKnownLengthUnparser(e.elementRuntimeData, signed, lengthInBits.toInt)
}

class BinaryIntegerPrefixedLength(val e: ElementBase, signed: Boolean) extends Terminal(e, true) {

  override lazy val parser =
    new BinaryIntegerPrefixedLengthParser(
      e.elementRuntimeData,
      e.prefixedLengthBody.parser,
      e.prefixedLengthElementDecl.elementRuntimeData,
      signed,
      e.lengthUnits,
      e.prefixedLengthAdjustmentInUnits)

  override lazy val unparser: Unparser = {
    val maybeNBits = e.primType match {
      case NodeInfo.Long | NodeInfo.UnsignedLong => MaybeInt(64)
      case NodeInfo.Int | NodeInfo.UnsignedInt => MaybeInt(32)
      case NodeInfo.Short | NodeInfo.UnsignedShort => MaybeInt(16)
      case NodeInfo.Byte | NodeInfo.UnsignedByte => MaybeInt(8)
      case NodeInfo.Integer | NodeInfo.NonNegativeInteger => MaybeInt.Nope
      case _ => Assert.invariantFailed("Only integer base types should be used for this primitive")
    }
    new BinaryIntegerPrefixedLengthUnparser(
      e.elementRuntimeData,
      e.prefixedLengthBody.unparser,
      e.prefixedLengthElementDecl.elementRuntimeData,
      maybeNBits,
      signed,
      e.lengthUnits,
      e.prefixedLengthAdjustmentInUnits)
  }
}

class BinaryDecimalRuntimeLength(val e: ElementBase) extends Terminal(e, true) {

  override lazy val parser = new BinaryDecimalRuntimeLengthParser(e.elementRuntimeData, e.decimalSigned, e.binaryDecimalVirtualPoint, e.lengthEv, e.lengthUnits)

  override lazy val unparser: Unparser = new BinaryDecimalRuntimeLengthUnparser(e.elementRuntimeData, e.decimalSigned, e.binaryDecimalVirtualPoint, e.lengthEv, e.lengthUnits)

}

class BinaryDecimalKnownLength(val e: ElementBase, lengthInBits: Long) extends Terminal(e, true) {

  override lazy val parser = new BinaryDecimalKnownLengthParser(e.elementRuntimeData, e.decimalSigned, e.binaryDecimalVirtualPoint, lengthInBits.toInt)

  override lazy val unparser: Unparser = new BinaryDecimalKnownLengthUnparser(e.elementRuntimeData, e.decimalSigned, e.binaryDecimalVirtualPoint, lengthInBits.toInt)
}

class BinaryDecimalPrefixedLength(val e: ElementBase) extends Terminal(e, true) {

  override lazy val parser =
    new BinaryDecimalPrefixedLengthParser(
      e.elementRuntimeData,
      e.prefixedLengthBody.parser,
      e.prefixedLengthElementDecl.elementRuntimeData,
      e.decimalSigned,
      e.binaryDecimalVirtualPoint,
      e.lengthUnits,
      e.prefixedLengthAdjustmentInUnits)

  override lazy val unparser: Unparser =
    new BinaryDecimalPrefixedLengthUnparser(
      e.elementRuntimeData,
      e.prefixedLengthBody.unparser,
      e.prefixedLengthElementDecl.elementRuntimeData,
      e.decimalSigned,
      e.binaryDecimalVirtualPoint,
      e.lengthUnits,
      e.prefixedLengthAdjustmentInUnits)

}

class BinaryFloat(val e: ElementBase) extends Terminal(e, true) {

  override lazy val parser = new BinaryFloatParser(e.elementRuntimeData)

  override lazy val unparser: Unparser = new BinaryFloatUnparser(e.elementRuntimeData)
}

class BinaryDouble(val e: ElementBase) extends Terminal(e, true) {

  override lazy val parser = new BinaryDoubleParser(e.elementRuntimeData)

  override lazy val unparser: Unparser = new BinaryDoubleUnparser(e.elementRuntimeData)
}
