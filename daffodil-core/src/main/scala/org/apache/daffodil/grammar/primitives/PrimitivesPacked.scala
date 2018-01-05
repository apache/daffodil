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

package org.apache.daffodil.grammar.primitives

import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.util.PackedSignCodes
import org.apache.daffodil.processors.parsers.PackedIntegerRuntimeLengthParser
import org.apache.daffodil.processors.parsers.PackedIntegerKnownLengthParser
import org.apache.daffodil.processors.parsers.PackedDecimalRuntimeLengthParser
import org.apache.daffodil.processors.parsers.PackedDecimalKnownLengthParser
import org.apache.daffodil.processors.unparsers.Unparser
import org.apache.daffodil.processors.unparsers.PackedIntegerRuntimeLengthUnparser
import org.apache.daffodil.processors.unparsers.PackedIntegerKnownLengthUnparser
import org.apache.daffodil.processors.unparsers.PackedDecimalRuntimeLengthUnparser
import org.apache.daffodil.processors.unparsers.PackedDecimalKnownLengthUnparser

class PackedIntegerRuntimeLength(val e: ElementBase, signed: Boolean, packedSignCodes: PackedSignCodes) extends Terminal(e, true) {
  override lazy val parser = new PackedIntegerRuntimeLengthParser(e.elementRuntimeData, signed, packedSignCodes, e.lengthEv, e.lengthUnits)

  override lazy val unparser: Unparser = new PackedIntegerRuntimeLengthUnparser(e.elementRuntimeData, packedSignCodes, e.lengthEv, e.lengthUnits)
}

class PackedIntegerKnownLength(val e: ElementBase, signed: Boolean, packedSignCodes: PackedSignCodes, lengthInBits: Long) extends Terminal(e, true) {

  override lazy val parser = new PackedIntegerKnownLengthParser(e.elementRuntimeData, signed, packedSignCodes, lengthInBits.toInt)

  override lazy val unparser: Unparser = new PackedIntegerKnownLengthUnparser(e.elementRuntimeData, packedSignCodes, lengthInBits.toInt)
}

class PackedDecimalRuntimeLength(val e: ElementBase, packedSignCodes: PackedSignCodes) extends Terminal(e, true) {
  override lazy val parser = new PackedDecimalRuntimeLengthParser(e.elementRuntimeData, e.binaryDecimalVirtualPoint, packedSignCodes, e.lengthEv, e.lengthUnits)

  override lazy val unparser: Unparser = new PackedDecimalRuntimeLengthUnparser(e.elementRuntimeData, e.binaryDecimalVirtualPoint, packedSignCodes, e.lengthEv, e.lengthUnits)

}

class PackedDecimalKnownLength(val e: ElementBase, packedSignCodes: PackedSignCodes, lengthInBits: Long) extends Terminal(e, true) {
  override lazy val parser = new PackedDecimalKnownLengthParser(e.elementRuntimeData, e.binaryDecimalVirtualPoint, packedSignCodes, lengthInBits.toInt)

  override lazy val unparser: Unparser = new PackedDecimalKnownLengthUnparser(e.elementRuntimeData, e.binaryDecimalVirtualPoint, packedSignCodes, lengthInBits.toInt)
}
