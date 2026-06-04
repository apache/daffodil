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
import org.apache.daffodil.runtime1.processors.parsers.BinaryBooleanBitLimitLengthParser
import org.apache.daffodil.runtime1.processors.parsers.BinaryBooleanParser
import org.apache.daffodil.runtime1.processors.unparsers.Unparser
import org.apache.daffodil.unparsers.runtime1.BinaryBooleanMinimumLengthUnparser
import org.apache.daffodil.unparsers.runtime1.BinaryBooleanUnparser

class BinaryBoolean(val e: ElementBase) extends Terminal(e, true) {
  override lazy val parser = new BinaryBooleanParser(
    e.elementRuntimeData,
    e.binaryBooleanTrueRep,
    e.binaryBooleanFalseRep,
    e.lengthEv,
    e.lengthUnits,
    e.lengthKind
  )

  override lazy val unparser: Unparser = new BinaryBooleanUnparser(
    e.elementRuntimeData,
    e.binaryBooleanTrueRep,
    e.binaryBooleanFalseRep,
    e.lengthEv,
    e.lengthUnits,
    e.lengthKind
  )
}

class BinaryBooleanPrefixedLength(val e: ElementBase) extends Terminal(e, true) {
  override lazy val parser = new BinaryBooleanBitLimitLengthParser(
    e.elementRuntimeData,
    e.binaryBooleanTrueRep,
    e.binaryBooleanFalseRep,
    e.lengthUnits
  )

  override lazy val unparser: Unparser = new BinaryBooleanMinimumLengthUnparser(
    e.elementRuntimeData,
    e.binaryBooleanTrueRep,
    e.binaryBooleanFalseRep,
    e.lengthUnits
  )
}
