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
import org.apache.daffodil.processors.parsers.BCDDecimalKnownLengthParser
import org.apache.daffodil.processors.parsers.BCDDecimalPrefixedLengthParser
import org.apache.daffodil.processors.parsers.BCDDecimalRuntimeLengthParser
import org.apache.daffodil.processors.parsers.BCDIntegerKnownLengthParser
import org.apache.daffodil.processors.parsers.BCDIntegerPrefixedLengthParser
import org.apache.daffodil.processors.parsers.BCDIntegerRuntimeLengthParser
import org.apache.daffodil.processors.unparsers.BCDDecimalKnownLengthUnparser
import org.apache.daffodil.processors.unparsers.BCDDecimalPrefixedLengthUnparser
import org.apache.daffodil.processors.unparsers.BCDDecimalRuntimeLengthUnparser
import org.apache.daffodil.processors.unparsers.BCDIntegerKnownLengthUnparser
import org.apache.daffodil.processors.unparsers.BCDIntegerPrefixedLengthUnparser
import org.apache.daffodil.processors.unparsers.BCDIntegerRuntimeLengthUnparser
import org.apache.daffodil.processors.unparsers.Unparser

class BCDIntegerRuntimeLength(val e: ElementBase) extends Terminal(e, true) {
  override lazy val parser = new BCDIntegerRuntimeLengthParser(e.elementRuntimeData, e.lengthEv, e.lengthUnits)

  override lazy val unparser: Unparser = new BCDIntegerRuntimeLengthUnparser(e.elementRuntimeData, e.lengthEv, e.lengthUnits)
}

class BCDIntegerKnownLength(val e: ElementBase, lengthInBits: Long) extends Terminal(e, true) {

  override lazy val parser = new BCDIntegerKnownLengthParser(e.elementRuntimeData, lengthInBits.toInt)

  override lazy val unparser: Unparser = new BCDIntegerKnownLengthUnparser(e.elementRuntimeData, lengthInBits.toInt)
}

class BCDIntegerPrefixedLength(val e: ElementBase) extends Terminal(e, true) {

  override lazy val parser = new BCDIntegerPrefixedLengthParser(
    e.elementRuntimeData,
    e.prefixedLengthBody.parser,
    e.prefixedLengthElementDecl.elementRuntimeData,
    e.lengthUnits,
    e.prefixedLengthAdjustmentInUnits)

  override lazy val unparser: Unparser = new BCDIntegerPrefixedLengthUnparser(
    e.elementRuntimeData,
    e.prefixedLengthBody.unparser,
    e.prefixedLengthElementDecl.elementRuntimeData,
    e.lengthUnits,
    e.prefixedLengthAdjustmentInUnits)
}


class BCDDecimalRuntimeLength(val e: ElementBase) extends Terminal(e, true) {
  override lazy val parser = new BCDDecimalRuntimeLengthParser(e.elementRuntimeData, e.binaryDecimalVirtualPoint, e.lengthEv, e.lengthUnits)

  override lazy val unparser: Unparser = new BCDDecimalRuntimeLengthUnparser(e.elementRuntimeData, e.binaryDecimalVirtualPoint, e.lengthEv, e.lengthUnits)

}

class BCDDecimalKnownLength(val e: ElementBase, lengthInBits: Long) extends Terminal(e, true) {
  override lazy val parser = new BCDDecimalKnownLengthParser(e.elementRuntimeData, e.binaryDecimalVirtualPoint, lengthInBits.toInt)

  override lazy val unparser: Unparser = new BCDDecimalKnownLengthUnparser(e.elementRuntimeData, e.binaryDecimalVirtualPoint, lengthInBits.toInt)
}

class BCDDecimalPrefixedLength(val e: ElementBase) extends Terminal(e, true) {

  override lazy val parser = new BCDDecimalPrefixedLengthParser(
    e.elementRuntimeData,
    e.prefixedLengthBody.parser,
    e.prefixedLengthElementDecl.elementRuntimeData,
    e.binaryDecimalVirtualPoint,
    e.lengthUnits,
    e.prefixedLengthAdjustmentInUnits)

  override lazy val unparser: Unparser = new BCDDecimalPrefixedLengthUnparser(
    e.elementRuntimeData,
    e.prefixedLengthBody.unparser,
    e.prefixedLengthElementDecl.elementRuntimeData,
    e.binaryDecimalVirtualPoint,
    e.lengthUnits,
    e.prefixedLengthAdjustmentInUnits)
}

