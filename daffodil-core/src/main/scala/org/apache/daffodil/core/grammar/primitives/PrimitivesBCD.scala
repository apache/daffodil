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
import org.apache.daffodil.runtime1.processors.parsers.BCDDecimalBitLimitLengthParser
import org.apache.daffodil.runtime1.processors.parsers.BCDDecimalKnownLengthParser
import org.apache.daffodil.runtime1.processors.parsers.BCDDecimalRuntimeLengthParser
import org.apache.daffodil.runtime1.processors.parsers.BCDIntegerBitLimitLengthParser
import org.apache.daffodil.runtime1.processors.parsers.BCDIntegerKnownLengthParser
import org.apache.daffodil.runtime1.processors.parsers.BCDIntegerRuntimeLengthParser
import org.apache.daffodil.runtime1.processors.unparsers.Unparser
import org.apache.daffodil.unparsers.runtime1.BCDDecimalKnownLengthUnparser
import org.apache.daffodil.unparsers.runtime1.BCDDecimalMinimumLengthUnparser
import org.apache.daffodil.unparsers.runtime1.BCDDecimalRuntimeLengthUnparser
import org.apache.daffodil.unparsers.runtime1.BCDIntegerKnownLengthUnparser
import org.apache.daffodil.unparsers.runtime1.BCDIntegerMinimumLengthUnparser
import org.apache.daffodil.unparsers.runtime1.BCDIntegerRuntimeLengthUnparser

class BCDIntegerRuntimeLength(val e: ElementBase) extends Terminal(e, true) {
  override lazy val parser =
    new BCDIntegerRuntimeLengthParser(e.elementRuntimeData, e.lengthEv, e.lengthUnits)

  override lazy val unparser: Unparser =
    new BCDIntegerRuntimeLengthUnparser(e.elementRuntimeData, e.lengthEv, e.lengthUnits)
}

class BCDIntegerKnownLength(val e: ElementBase, lengthInBits: Long) extends Terminal(e, true) {

  override lazy val parser =
    new BCDIntegerKnownLengthParser(e.elementRuntimeData, lengthInBits.toInt)

  override lazy val unparser: Unparser =
    new BCDIntegerKnownLengthUnparser(e.elementRuntimeData, lengthInBits.toInt)
}

class BCDIntegerPrefixedLength(val e: ElementBase) extends Terminal(e, true) {

  override lazy val parser = new BCDIntegerBitLimitLengthParser(e.elementRuntimeData)

  override lazy val unparser: Unparser = new BCDIntegerMinimumLengthUnparser(
    e.elementRuntimeData
  )
}

class BCDDecimalRuntimeLength(val e: ElementBase) extends Terminal(e, true) {
  override lazy val parser = new BCDDecimalRuntimeLengthParser(
    e.elementRuntimeData,
    e.binaryDecimalVirtualPoint,
    e.lengthEv,
    e.lengthUnits
  )

  override lazy val unparser: Unparser = new BCDDecimalRuntimeLengthUnparser(
    e.elementRuntimeData,
    e.binaryDecimalVirtualPoint,
    e.lengthEv,
    e.lengthUnits
  )

}

class BCDDecimalKnownLength(val e: ElementBase, lengthInBits: Long) extends Terminal(e, true) {
  override lazy val parser = new BCDDecimalKnownLengthParser(
    e.elementRuntimeData,
    e.binaryDecimalVirtualPoint,
    lengthInBits.toInt
  )

  override lazy val unparser: Unparser = new BCDDecimalKnownLengthUnparser(
    e.elementRuntimeData,
    e.binaryDecimalVirtualPoint,
    lengthInBits.toInt
  )
}

class BCDDecimalPrefixedLength(val e: ElementBase) extends Terminal(e, true) {

  override lazy val parser =
    new BCDDecimalBitLimitLengthParser(e.elementRuntimeData, e.binaryDecimalVirtualPoint)

  override lazy val unparser: Unparser =
    new BCDDecimalMinimumLengthUnparser(e.elementRuntimeData, e.binaryDecimalVirtualPoint)
}
