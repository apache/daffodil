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

package org.apache.daffodil.core.grammar.primitives

import org.apache.daffodil.core.dsom.ElementBase
import org.apache.daffodil.core.grammar.Terminal
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.MaybeInt
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.processors.parsers.BinaryDecimalBitLimitLengthParser
import org.apache.daffodil.runtime1.processors.parsers.BinaryDecimalKnownLengthParser
import org.apache.daffodil.runtime1.processors.parsers.BinaryDecimalRuntimeLengthParser
import org.apache.daffodil.runtime1.processors.parsers.BinaryDoubleParser
import org.apache.daffodil.runtime1.processors.parsers.BinaryFloatParser
import org.apache.daffodil.runtime1.processors.parsers.BinaryIntegerBitLimitLengthParser
import org.apache.daffodil.runtime1.processors.parsers.BinaryIntegerKnownLengthParser
import org.apache.daffodil.runtime1.processors.parsers.BinaryIntegerRuntimeLengthParser
import org.apache.daffodil.runtime1.processors.unparsers.Unparser
import org.apache.daffodil.unparsers.runtime1.BinaryDecimalKnownLengthUnparser
import org.apache.daffodil.unparsers.runtime1.BinaryDecimalMinimumLengthUnparser
import org.apache.daffodil.unparsers.runtime1.BinaryDecimalRuntimeLengthUnparser
import org.apache.daffodil.unparsers.runtime1.BinaryDoubleUnparser
import org.apache.daffodil.unparsers.runtime1.BinaryFloatUnparser
import org.apache.daffodil.unparsers.runtime1.BinaryIntegerKnownLengthUnparser
import org.apache.daffodil.unparsers.runtime1.BinaryIntegerMinimumLengthUnparser
import org.apache.daffodil.unparsers.runtime1.BinaryIntegerRuntimeLengthUnparser

class BinaryIntegerRuntimeLength(val e: ElementBase) extends Terminal(e, true) {

  override lazy val parser = new BinaryIntegerRuntimeLengthParser(
    e.elementRuntimeData,
    e.lengthEv,
    e.lengthUnits
  )

  override lazy val unparser: Unparser = new BinaryIntegerRuntimeLengthUnparser(
    e.elementRuntimeData,
    e.lengthEv,
    e.lengthUnits
  )
}

class BinaryIntegerKnownLength(val e: ElementBase, val lengthInBits: Long)
  extends Terminal(e, true) {

  override lazy val parser = {
    new BinaryIntegerKnownLengthParser(e.elementRuntimeData, lengthInBits.toInt)
  }

  override lazy val unparser: Unparser =
    new BinaryIntegerKnownLengthUnparser(e.elementRuntimeData, lengthInBits.toInt)
}

class BinaryIntegerPrefixedLength(val e: ElementBase) extends Terminal(e, true) {

  private lazy val erd = e.elementRuntimeData

  override lazy val parser =
    new BinaryIntegerBitLimitLengthParser(erd)

  override lazy val unparser: Unparser = {
    val maybeNBits = e.primType match {
      case NodeInfo.Long | NodeInfo.UnsignedLong => MaybeInt(64)
      case NodeInfo.Int | NodeInfo.UnsignedInt => MaybeInt(32)
      case NodeInfo.Short | NodeInfo.UnsignedShort => MaybeInt(16)
      case NodeInfo.Byte | NodeInfo.UnsignedByte => MaybeInt(8)
      case NodeInfo.Integer | NodeInfo.NonNegativeInteger => MaybeInt.Nope
      case _ =>
        Assert.invariantFailed("Only integer base types should be used for this primitive")
    }
    new BinaryIntegerMinimumLengthUnparser(erd, maybeNBits)
  }
}

class BinaryDecimalRuntimeLength(val e: ElementBase) extends Terminal(e, true) {

  override lazy val parser = new BinaryDecimalRuntimeLengthParser(
    e.elementRuntimeData,
    e.decimalSigned,
    e.binaryDecimalVirtualPoint,
    e.lengthEv,
    e.lengthUnits
  )

  override lazy val unparser: Unparser = new BinaryDecimalRuntimeLengthUnparser(
    e.elementRuntimeData,
    e.decimalSigned,
    e.binaryDecimalVirtualPoint,
    e.lengthEv,
    e.lengthUnits
  )

}

class BinaryDecimalKnownLength(val e: ElementBase, lengthInBits: Long)
  extends Terminal(e, true) {

  override lazy val parser = new BinaryDecimalKnownLengthParser(
    e.elementRuntimeData,
    e.decimalSigned,
    e.binaryDecimalVirtualPoint,
    lengthInBits.toInt
  )

  override lazy val unparser: Unparser = new BinaryDecimalKnownLengthUnparser(
    e.elementRuntimeData,
    e.decimalSigned,
    e.binaryDecimalVirtualPoint,
    lengthInBits.toInt
  )
}

class BinaryDecimalPrefixedLength(val e: ElementBase) extends Terminal(e, true) {

  override lazy val parser =
    new BinaryDecimalBitLimitLengthParser(
      e.elementRuntimeData,
      e.decimalSigned,
      e.binaryDecimalVirtualPoint
    )

  override lazy val unparser: Unparser =
    new BinaryDecimalMinimumLengthUnparser(
      e.elementRuntimeData,
      e.decimalSigned,
      e.binaryDecimalVirtualPoint
    )

}

class BinaryFloat(val e: ElementBase) extends Terminal(e, true) {

  override lazy val parser = new BinaryFloatParser(e.elementRuntimeData)

  override lazy val unparser: Unparser = new BinaryFloatUnparser(e.elementRuntimeData)
}

class BinaryDouble(val e: ElementBase) extends Terminal(e, true) {

  override lazy val parser = new BinaryDoubleParser(e.elementRuntimeData)

  override lazy val unparser: Unparser = new BinaryDoubleUnparser(e.elementRuntimeData)
}
