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
import edu.illinois.ncsa.daffodil.processors.parsers.BCDIntegerRuntimeLengthParser
import edu.illinois.ncsa.daffodil.processors.parsers.BCDIntegerKnownLengthParser
import edu.illinois.ncsa.daffodil.processors.parsers.BCDDecimalRuntimeLengthParser
import edu.illinois.ncsa.daffodil.processors.parsers.BCDDecimalKnownLengthParser
import edu.illinois.ncsa.daffodil.processors.unparsers.Unparser
import edu.illinois.ncsa.daffodil.processors.unparsers.BCDIntegerRuntimeLengthUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.BCDIntegerKnownLengthUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.BCDDecimalRuntimeLengthUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.BCDDecimalKnownLengthUnparser

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
