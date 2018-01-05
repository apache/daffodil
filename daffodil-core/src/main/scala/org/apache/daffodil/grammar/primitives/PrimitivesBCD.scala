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
import org.apache.daffodil.processors.parsers.BCDIntegerRuntimeLengthParser
import org.apache.daffodil.processors.parsers.BCDIntegerKnownLengthParser
import org.apache.daffodil.processors.parsers.BCDDecimalRuntimeLengthParser
import org.apache.daffodil.processors.parsers.BCDDecimalKnownLengthParser
import org.apache.daffodil.processors.unparsers.Unparser
import org.apache.daffodil.processors.unparsers.BCDIntegerRuntimeLengthUnparser
import org.apache.daffodil.processors.unparsers.BCDIntegerKnownLengthUnparser
import org.apache.daffodil.processors.unparsers.BCDDecimalRuntimeLengthUnparser
import org.apache.daffodil.processors.unparsers.BCDDecimalKnownLengthUnparser

class BCDIntegerRuntimeLength(val e: ElementBase) extends Terminal(e, true) {
  override lazy val parser = new BCDIntegerRuntimeLengthParser(e.elementRuntimeData, e.lengthEv, e.lengthUnits)

  override lazy val unparser: Unparser = new BCDIntegerRuntimeLengthUnparser(e.elementRuntimeData, e.lengthEv, e.lengthUnits)
}

class BCDIntegerKnownLength(val e: ElementBase, lengthInBits: Long) extends Terminal(e, true) {

  override lazy val parser = new BCDIntegerKnownLengthParser(e.elementRuntimeData, lengthInBits.toInt)

  override lazy val unparser: Unparser = new BCDIntegerKnownLengthUnparser(e.elementRuntimeData, lengthInBits.toInt)
}

class BCDDecimalRuntimeLength(val e: ElementBase) extends Terminal(e, true) {
  override lazy val parser = new BCDDecimalRuntimeLengthParser(e.elementRuntimeData, e.binaryDecimalVirtualPoint, e.lengthEv, e.lengthUnits)

  override lazy val unparser: Unparser = new BCDDecimalRuntimeLengthUnparser(e.elementRuntimeData, e.binaryDecimalVirtualPoint, e.lengthEv, e.lengthUnits)

}

class BCDDecimalKnownLength(val e: ElementBase, lengthInBits: Long) extends Terminal(e, true) {
  override lazy val parser = new BCDDecimalKnownLengthParser(e.elementRuntimeData, e.binaryDecimalVirtualPoint, lengthInBits.toInt)

  override lazy val unparser: Unparser = new BCDDecimalKnownLengthUnparser(e.elementRuntimeData, e.binaryDecimalVirtualPoint, lengthInBits.toInt)
}
