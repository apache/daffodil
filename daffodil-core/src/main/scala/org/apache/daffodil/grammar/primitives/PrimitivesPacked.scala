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

import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.processors.parsers.PackedDecimalKnownLengthParser
import org.apache.daffodil.processors.parsers.PackedDecimalPrefixedLengthParser
import org.apache.daffodil.processors.parsers.PackedDecimalRuntimeLengthParser
import org.apache.daffodil.processors.parsers.PackedIntegerKnownLengthParser
import org.apache.daffodil.processors.parsers.PackedIntegerPrefixedLengthParser
import org.apache.daffodil.processors.parsers.PackedIntegerRuntimeLengthParser
import org.apache.daffodil.processors.unparsers.PackedDecimalKnownLengthUnparser
import org.apache.daffodil.processors.unparsers.PackedDecimalPrefixedLengthUnparser
import org.apache.daffodil.processors.unparsers.PackedDecimalRuntimeLengthUnparser
import org.apache.daffodil.processors.unparsers.PackedIntegerKnownLengthUnparser
import org.apache.daffodil.processors.unparsers.PackedIntegerPrefixedLengthUnparser
import org.apache.daffodil.processors.unparsers.PackedIntegerRuntimeLengthUnparser
import org.apache.daffodil.processors.unparsers.Unparser
import org.apache.daffodil.util.PackedSignCodes

class PackedIntegerRuntimeLength(val e: ElementBase, signed: Boolean, packedSignCodes: PackedSignCodes) extends Terminal(e, true) {
  override lazy val parser = new PackedIntegerRuntimeLengthParser(e.elementRuntimeData, signed, packedSignCodes, e.lengthEv, e.lengthUnits)

  override lazy val unparser: Unparser = new PackedIntegerRuntimeLengthUnparser(e.elementRuntimeData, packedSignCodes, e.lengthEv, e.lengthUnits)
}

class PackedIntegerKnownLength(val e: ElementBase, signed: Boolean, packedSignCodes: PackedSignCodes, lengthInBits: Long) extends Terminal(e, true) {

  override lazy val parser = new PackedIntegerKnownLengthParser(e.elementRuntimeData, signed, packedSignCodes, lengthInBits.toInt)

  override lazy val unparser: Unparser = new PackedIntegerKnownLengthUnparser(e.elementRuntimeData, packedSignCodes, lengthInBits.toInt)
}

class PackedIntegerPrefixedLength(val e: ElementBase, signed: Boolean, packedSignCodes: PackedSignCodes) extends Terminal(e, true) {

  override lazy val parser = new PackedIntegerPrefixedLengthParser(
    e.elementRuntimeData,
    e.prefixedLengthBody.parser,
    e.prefixedLengthElementDecl.elementRuntimeData,
    signed,
    packedSignCodes,
    e.lengthUnits,
    e.prefixedLengthAdjustmentInUnits)

  override lazy val unparser: Unparser = new PackedIntegerPrefixedLengthUnparser(
    e.elementRuntimeData,
    e.prefixedLengthBody.unparser,
    e.prefixedLengthElementDecl.elementRuntimeData,
    packedSignCodes,
    e.lengthUnits,
    e.prefixedLengthAdjustmentInUnits)
}

class PackedDecimalRuntimeLength(val e: ElementBase, packedSignCodes: PackedSignCodes) extends Terminal(e, true) {
  override lazy val parser = new PackedDecimalRuntimeLengthParser(e.elementRuntimeData, e.binaryDecimalVirtualPoint, packedSignCodes, e.lengthEv, e.lengthUnits)

  override lazy val unparser: Unparser = new PackedDecimalRuntimeLengthUnparser(e.elementRuntimeData, e.binaryDecimalVirtualPoint, packedSignCodes, e.lengthEv, e.lengthUnits)

}

class PackedDecimalKnownLength(val e: ElementBase, packedSignCodes: PackedSignCodes, lengthInBits: Long) extends Terminal(e, true) {
  override lazy val parser = new PackedDecimalKnownLengthParser(e.elementRuntimeData, e.binaryDecimalVirtualPoint, packedSignCodes, lengthInBits.toInt)

  override lazy val unparser: Unparser = new PackedDecimalKnownLengthUnparser(e.elementRuntimeData, e.binaryDecimalVirtualPoint, packedSignCodes, lengthInBits.toInt)
}


class PackedDecimalPrefixedLength(val e: ElementBase, packedSignCodes: PackedSignCodes) extends Terminal(e, true) {

  override lazy val parser = new PackedDecimalPrefixedLengthParser(
    e.elementRuntimeData,
    e.prefixedLengthBody.parser,
    e.prefixedLengthElementDecl.elementRuntimeData,
    e.binaryDecimalVirtualPoint,
    packedSignCodes,
    e.lengthUnits,
    e.prefixedLengthAdjustmentInUnits)

  override lazy val unparser: Unparser = new PackedDecimalPrefixedLengthUnparser(
    e.elementRuntimeData,
    e.prefixedLengthBody.unparser,
    e.prefixedLengthElementDecl.elementRuntimeData,
    e.binaryDecimalVirtualPoint,
    packedSignCodes,
    e.lengthUnits,
    e.prefixedLengthAdjustmentInUnits)
}
