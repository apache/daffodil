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

import org.apache.daffodil.core.dsom.ModelGroup
import org.apache.daffodil.core.grammar.Gram
import org.apache.daffodil.core.grammar.Terminal
import org.apache.daffodil.runtime1.processors.parsers.HiddenGroupCombinatorParser
import org.apache.daffodil.runtime1.processors.parsers.Parser
import org.apache.daffodil.runtime1.processors.unparsers.Unparser
import org.apache.daffodil.unparsers.runtime1.HiddenGroupCombinatorUnparser

final class HiddenGroupCombinator(ctxt: ModelGroup, body: Gram)
  extends Terminal(ctxt, !body.isEmpty) {

  lazy val parser: Parser =
    new HiddenGroupCombinatorParser(ctxt.modelGroupRuntimeData, body.parser)

  override lazy val unparser: Unparser =
    new HiddenGroupCombinatorUnparser(ctxt.modelGroupRuntimeData, body.unparser)

}
