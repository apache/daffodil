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
import org.apache.daffodil.runtime1.processors.parsers.IBM4690PackedDecimalBitLimitLengthParser
import org.apache.daffodil.runtime1.processors.parsers.IBM4690PackedDecimalKnownLengthParser
import org.apache.daffodil.runtime1.processors.parsers.IBM4690PackedDecimalRuntimeLengthParser
import org.apache.daffodil.runtime1.processors.parsers.IBM4690PackedIntegerBitLimitLengthParser
import org.apache.daffodil.runtime1.processors.parsers.IBM4690PackedIntegerKnownLengthParser
import org.apache.daffodil.runtime1.processors.parsers.IBM4690PackedIntegerRuntimeLengthParser
import org.apache.daffodil.runtime1.processors.unparsers.Unparser
import org.apache.daffodil.unparsers.runtime1.IBM4690PackedDecimalKnownLengthUnparser
import org.apache.daffodil.unparsers.runtime1.IBM4690PackedDecimalMinimumLengthUnparser
import org.apache.daffodil.unparsers.runtime1.IBM4690PackedDecimalRuntimeLengthUnparser
import org.apache.daffodil.unparsers.runtime1.IBM4690PackedIntegerKnownLengthUnparser
import org.apache.daffodil.unparsers.runtime1.IBM4690PackedIntegerMinimumLengthUnparser
import org.apache.daffodil.unparsers.runtime1.IBM4690PackedIntegerRuntimeLengthUnparser

class IBM4690PackedIntegerRuntimeLength(val e: ElementBase) extends Terminal(e, true) {
  override lazy val parser = new IBM4690PackedIntegerRuntimeLengthParser(
    e.elementRuntimeData,
    e.lengthEv,
    e.lengthUnits
  )

  override lazy val unparser: Unparser = new IBM4690PackedIntegerRuntimeLengthUnparser(
    e.elementRuntimeData,
    e.lengthEv,
    e.lengthUnits
  )
}

class IBM4690PackedIntegerKnownLength(val e: ElementBase, lengthInBits: Long)
  extends Terminal(e, true) {

  override lazy val parser =
    new IBM4690PackedIntegerKnownLengthParser(e.elementRuntimeData, lengthInBits.toInt)

  override lazy val unparser: Unparser =
    new IBM4690PackedIntegerKnownLengthUnparser(e.elementRuntimeData, lengthInBits.toInt)
}

class IBM4690PackedIntegerPrefixedLength(val e: ElementBase) extends Terminal(e, true) {
  override lazy val parser =
    new IBM4690PackedIntegerBitLimitLengthParser(e.elementRuntimeData)

  override lazy val unparser: Unparser = new IBM4690PackedIntegerMinimumLengthUnparser(
    e.elementRuntimeData
  )
}

class IBM4690PackedDecimalRuntimeLength(val e: ElementBase) extends Terminal(e, true) {
  override lazy val parser = new IBM4690PackedDecimalRuntimeLengthParser(
    e.elementRuntimeData,
    e.binaryDecimalVirtualPoint,
    e.lengthEv,
    e.lengthUnits
  )

  override lazy val unparser: Unparser = new IBM4690PackedDecimalRuntimeLengthUnparser(
    e.elementRuntimeData,
    e.binaryDecimalVirtualPoint,
    e.lengthEv,
    e.lengthUnits
  )

}

class IBM4690PackedDecimalKnownLength(val e: ElementBase, lengthInBits: Long)
  extends Terminal(e, true) {
  override lazy val parser = new IBM4690PackedDecimalKnownLengthParser(
    e.elementRuntimeData,
    e.binaryDecimalVirtualPoint,
    lengthInBits.toInt
  )

  override lazy val unparser: Unparser = new IBM4690PackedDecimalKnownLengthUnparser(
    e.elementRuntimeData,
    e.binaryDecimalVirtualPoint,
    lengthInBits.toInt
  )
}

class IBM4690PackedDecimalPrefixedLength(val e: ElementBase) extends Terminal(e, true) {
  override lazy val parser = new IBM4690PackedDecimalBitLimitLengthParser(
    e.elementRuntimeData,
    e.binaryDecimalVirtualPoint
  )

  override lazy val unparser: Unparser = new IBM4690PackedDecimalMinimumLengthUnparser(
    e.elementRuntimeData,
    e.binaryDecimalVirtualPoint
  )
}
