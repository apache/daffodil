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

import java.math.{ BigInteger => JBigInt }

import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.processors.parsers.Parser
import org.apache.daffodil.processors.unparsers.Unparser
import org.apache.daffodil.processors.parsers.ConvertNonBaseTenTextNumberParser
import org.apache.daffodil.processors.unparsers.ConvertNonBaseTenTextNumberUnparser

import org.apache.daffodil.processors.unparsers.NadaUnparser


case class ConvertNonBaseTenTextNumberPrim(e: ElementBase)
  extends Terminal(e, true) {

  override lazy val parser: Parser =
    new ConvertNonBaseTenTextNumberParser(
      e.elementRuntimeData,
      e.textStandardBaseDefaulted)

  override lazy val unparser: Unparser =
    new ConvertNonBaseTenTextNumberUnparser(
    e.elementRuntimeData,
    e.textStandardBaseDefaulted)
}
