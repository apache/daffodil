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
import org.apache.daffodil.lib.util.PackedSignCodes
import org.apache.daffodil.runtime1.processors.parsers.PackedDecimalBitLimitLengthParser
import org.apache.daffodil.runtime1.processors.parsers.PackedDecimalKnownLengthParser
import org.apache.daffodil.runtime1.processors.parsers.PackedDecimalRuntimeLengthParser
import org.apache.daffodil.runtime1.processors.parsers.PackedIntegerBitLimitLengthParser
import org.apache.daffodil.runtime1.processors.parsers.PackedIntegerKnownLengthParser
import org.apache.daffodil.runtime1.processors.parsers.PackedIntegerRuntimeLengthParser
import org.apache.daffodil.runtime1.processors.unparsers.Unparser
import org.apache.daffodil.unparsers.runtime1.PackedDecimalKnownLengthUnparser
import org.apache.daffodil.unparsers.runtime1.PackedDecimalMinimumLengthUnparser
import org.apache.daffodil.unparsers.runtime1.PackedDecimalRuntimeLengthUnparser
import org.apache.daffodil.unparsers.runtime1.PackedIntegerKnownLengthUnparser
import org.apache.daffodil.unparsers.runtime1.PackedIntegerMinimumLengthUnparser
import org.apache.daffodil.unparsers.runtime1.PackedIntegerRuntimeLengthUnparser

class PackedIntegerRuntimeLength(
  val e: ElementBase,
  packedSignCodes: PackedSignCodes
) extends Terminal(e, true) {
  override lazy val parser = new PackedIntegerRuntimeLengthParser(
    e.elementRuntimeData,
    packedSignCodes,
    e.lengthEv,
    e.lengthUnits
  )

  override lazy val unparser: Unparser = new PackedIntegerRuntimeLengthUnparser(
    e.elementRuntimeData,
    packedSignCodes,
    e.lengthEv,
    e.lengthUnits
  )
}

class PackedIntegerKnownLength(
  val e: ElementBase,
  packedSignCodes: PackedSignCodes,
  lengthInBits: Long
) extends Terminal(e, true) {

  override lazy val parser = new PackedIntegerKnownLengthParser(
    e.elementRuntimeData,
    packedSignCodes,
    lengthInBits.toInt
  )

  override lazy val unparser: Unparser = new PackedIntegerKnownLengthUnparser(
    e.elementRuntimeData,
    packedSignCodes,
    lengthInBits.toInt
  )
}

class PackedIntegerPrefixedLength(
  val e: ElementBase,
  packedSignCodes: PackedSignCodes
) extends Terminal(e, true) {

  override lazy val parser =
    new PackedIntegerBitLimitLengthParser(e.elementRuntimeData, packedSignCodes)

  override lazy val unparser: Unparser =
    new PackedIntegerMinimumLengthUnparser(e.elementRuntimeData, packedSignCodes)
}

class PackedDecimalRuntimeLength(val e: ElementBase, packedSignCodes: PackedSignCodes)
  extends Terminal(e, true) {
  override lazy val parser = new PackedDecimalRuntimeLengthParser(
    e.elementRuntimeData,
    e.binaryDecimalVirtualPoint,
    packedSignCodes,
    e.lengthEv,
    e.lengthUnits
  )

  override lazy val unparser: Unparser = new PackedDecimalRuntimeLengthUnparser(
    e.elementRuntimeData,
    e.binaryDecimalVirtualPoint,
    packedSignCodes,
    e.lengthEv,
    e.lengthUnits
  )

}

class PackedDecimalKnownLength(
  val e: ElementBase,
  packedSignCodes: PackedSignCodes,
  lengthInBits: Long
) extends Terminal(e, true) {
  override lazy val parser = new PackedDecimalKnownLengthParser(
    e.elementRuntimeData,
    e.binaryDecimalVirtualPoint,
    packedSignCodes,
    lengthInBits.toInt
  )

  override lazy val unparser: Unparser = new PackedDecimalKnownLengthUnparser(
    e.elementRuntimeData,
    e.binaryDecimalVirtualPoint,
    packedSignCodes,
    lengthInBits.toInt
  )
}

class PackedDecimalPrefixedLength(val e: ElementBase, packedSignCodes: PackedSignCodes)
  extends Terminal(e, true) {

  override lazy val parser = new PackedDecimalBitLimitLengthParser(
    e.elementRuntimeData,
    e.binaryDecimalVirtualPoint,
    packedSignCodes
  )

  override lazy val unparser: Unparser = new PackedDecimalMinimumLengthUnparser(
    e.elementRuntimeData,
    e.binaryDecimalVirtualPoint,
    packedSignCodes
  )
}
