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

import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.dsom.Term
import org.apache.daffodil.grammar.HasNoUnparser
import org.apache.daffodil.processors.parsers.NadaParser

case class Nada(sc: Term) extends Terminal(sc, true) with HasNoUnparser {
  // cannot optimize this out! It is used as an alternative to things
  // with the intention of "find this and this, or find nothing"

  override lazy val parser = new NadaParser(sc.runtimeData)
}
