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

import org.apache.daffodil.core.dsom._
import org.apache.daffodil.core.grammar.Terminal
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.runtime1.processors.parsers.LiteralCharacterNilOfSpecifiedLengthParser
import org.apache.daffodil.runtime1.processors.parsers.LiteralValueNilOfSpecifiedLengthParser
import org.apache.daffodil.runtime1.processors.unparsers.Unparser
import org.apache.daffodil.unparsers.runtime1.LiteralValueNilOfSpecifiedLengthUnparser
import org.apache.daffodil.unparsers.runtime1.NilLiteralCharacterUnparser

case class LiteralValueNilOfSpecifiedLength(e: ElementBase)
  extends Terminal(e, true)
  with Padded {

  override lazy val parser = new LiteralValueNilOfSpecifiedLengthParser(
    e.cookedNilValuesForParse,
    parsingPadChar,
    justificationTrim,
    e.ignoreCaseBool,
    e.elementRuntimeData
  )

  override lazy val unparser = new LiteralValueNilOfSpecifiedLengthUnparser(
    e.elementRuntimeData,
    e.nilStringLiteralForUnparserEv,
    e.lengthKind == LengthKind.Pattern
  )
}

case class LiteralCharacterNilOfSpecifiedLength(e: ElementBase)
  extends Terminal(e, true)
  with Padded {

  override lazy val parser = new LiteralCharacterNilOfSpecifiedLengthParser(
    e.cookedNilValuesForParse,
    parsingPadChar,
    justificationTrim,
    e.ignoreCaseBool,
    e.elementRuntimeData
  )

  private lazy val nilLitCharacter = e.cookedNilValuesForUnparse.head(0)

  override lazy val unparser: Unparser =
    new NilLiteralCharacterUnparser(
      e.erd,
      e.maybeUnparseTargetLengthInBitsEv.get,
      e.maybeLengthEv,
      e.maybeCharsetEv,
      nilLitCharacter
    )
}

case class LogicalNilValue(e: ElementBase) extends UnimplementedPrimitive(e, e.isNillable)
