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
import org.apache.daffodil.core.grammar.Gram
import org.apache.daffodil.core.grammar.Terminal
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.runtime1.processors.parsers.ComplexNilOrContentParser
import org.apache.daffodil.runtime1.processors.parsers.SimpleNilOrValueParser
import org.apache.daffodil.unparsers.runtime1.ComplexNilOrContentUnparser
import org.apache.daffodil.unparsers.runtime1.SimpleNilOrValueUnparser

case class SimpleNilOrValue(ctxt: ElementBase, nilGram: Gram, valueGram: Gram)
  extends Terminal(ctxt, true) {
  Assert.invariant(!nilGram.isEmpty)
  Assert.invariant(!valueGram.isEmpty)

  lazy val nilParser = nilGram.parser
  lazy val valueParser = valueGram.parser

  lazy val nilUnparser = nilGram.unparser
  lazy val valueUnparser = valueGram.unparser

  override lazy val parser = SimpleNilOrValueParser(ctxt.erd, nilParser, valueParser)

  override lazy val unparser = SimpleNilOrValueUnparser(ctxt.erd, nilUnparser, valueUnparser)

}

case class ComplexNilOrContent(ctxt: ElementBase, nilGram: Gram, contentGram: Gram)
  extends Terminal(ctxt, true) {
  Assert.invariant(!nilGram.isEmpty)
  Assert.invariant(!contentGram.isEmpty)

  lazy val nilParser = nilGram.parser
  lazy val contentParser = contentGram.parser

  lazy val nilUnparser = nilGram.unparser
  lazy val contentUnparser = contentGram.unparser

  override lazy val parser = ComplexNilOrContentParser(ctxt.erd, nilParser, contentParser)

  override lazy val unparser =
    ComplexNilOrContentUnparser(ctxt.erd, nilUnparser, contentUnparser)

}
