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


package edu.illinois.ncsa.daffodil.grammar.primitives

import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.processors.parsers.IBM4690PackedIntegerRuntimeLengthParser
import edu.illinois.ncsa.daffodil.processors.parsers.IBM4690PackedIntegerKnownLengthParser
import edu.illinois.ncsa.daffodil.processors.parsers.IBM4690PackedDecimalRuntimeLengthParser
import edu.illinois.ncsa.daffodil.processors.parsers.IBM4690PackedDecimalKnownLengthParser
import edu.illinois.ncsa.daffodil.processors.unparsers.Unparser
import edu.illinois.ncsa.daffodil.processors.unparsers.IBM4690PackedIntegerRuntimeLengthUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.IBM4690PackedIntegerKnownLengthUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.IBM4690PackedDecimalRuntimeLengthUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.IBM4690PackedDecimalKnownLengthUnparser

class IBM4690PackedIntegerRuntimeLength(val e: ElementBase, signed: Boolean) extends Terminal(e, true) {
  override lazy val parser = new IBM4690PackedIntegerRuntimeLengthParser(e.elementRuntimeData, signed, e.lengthEv, e.lengthUnits)

  override lazy val unparser: Unparser = new IBM4690PackedIntegerRuntimeLengthUnparser(e.elementRuntimeData, e.lengthEv, e.lengthUnits)
}

class IBM4690PackedIntegerKnownLength(val e: ElementBase, signed: Boolean, lengthInBits: Long) extends Terminal(e, true) {

  override lazy val parser = new IBM4690PackedIntegerKnownLengthParser(e.elementRuntimeData, signed, lengthInBits.toInt)

  override lazy val unparser: Unparser = new IBM4690PackedIntegerKnownLengthUnparser(e.elementRuntimeData, lengthInBits.toInt)
}

class IBM4690PackedDecimalRuntimeLength(val e: ElementBase) extends Terminal(e, true) {
  override lazy val parser = new IBM4690PackedDecimalRuntimeLengthParser(e.elementRuntimeData, e.binaryDecimalVirtualPoint, e.lengthEv, e.lengthUnits)

  override lazy val unparser: Unparser = new IBM4690PackedDecimalRuntimeLengthUnparser(e.elementRuntimeData, e.binaryDecimalVirtualPoint, e.lengthEv, e.lengthUnits)

}

class IBM4690PackedDecimalKnownLength(val e: ElementBase, lengthInBits: Long) extends Terminal(e, true) {
  override lazy val parser = new IBM4690PackedDecimalKnownLengthParser(e.elementRuntimeData, e.binaryDecimalVirtualPoint, lengthInBits.toInt)

  override lazy val unparser: Unparser = new IBM4690PackedDecimalKnownLengthUnparser(e.elementRuntimeData, e.binaryDecimalVirtualPoint, lengthInBits.toInt)
}

