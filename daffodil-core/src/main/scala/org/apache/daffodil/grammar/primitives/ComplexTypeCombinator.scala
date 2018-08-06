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
import org.apache.daffodil.dsom._
import org.apache.daffodil.processors.parsers.{ Parser => DaffodilParser }
import org.apache.daffodil.processors.unparsers.{ Unparser => DaffodilUnparser }
import org.apache.daffodil.processors.parsers._
import org.apache.daffodil.processors.unparsers._
import org.apache.daffodil.grammar.Gram
import org.apache.daffodil.util.Misc

case class ComplexTypeCombinator(ct: ComplexTypeBase, body: Gram) extends Terminal(ct.elementDecl, !body.isEmpty) {

  override def isEmpty = body.isEmpty

  private lazy val p = body.parser
  private lazy val u = body.unparser

  override def toString() =
    "<" + Misc.getNameFromClass(this) + ">" +
      body.toString() +
      "</" + Misc.getNameFromClass(this) + ">"

  override lazy val parser: DaffodilParser =
    if (p.isEmpty)
      p
    else
      new ComplexTypeParser(ct.runtimeData, p)

  override lazy val unparser: DaffodilUnparser =
    if (u.isEmpty)
      u
    else
      new ComplexTypeUnparser(ct.runtimeData, u)
}

