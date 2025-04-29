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

package org.apache.daffodil.core.runtime1

import org.apache.daffodil.core.grammar.Gram
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.runtime1.processors.parsers.Parser
import org.apache.daffodil.runtime1.processors.unparsers.Unparser

trait GramRuntime1Mixin { self: Gram =>

  /**
   * Provides parser.
   *
   * Required to examine child parsers, and optimize itself out by propagating NadaParser if there is no parser.
   */
  def parser: Parser

  final def maybeParser: Maybe[Parser] = {
    if (this.isEmpty) Maybe.Nope
    else {
      val p = this.parser
      if (p.isEmpty) Maybe.Nope
      else Maybe(p)
    }
  }

  protected final def hasNoParser: Parser = Assert.invariantFailed("Has no parser.")
  protected final def hasNoUnparser: Unparser = Assert.invariantFailed("Has no unparser.")

  /**
   * Provides unparser.
   *
   * Required to examine child unparsers, and optimize itself out by propagating NadaUnparser if there is no unparser.
   */
  def unparser: Unparser

  final lazy val maybeUnparser: Maybe[Unparser] = {
    if (this.isEmpty) Maybe.Nope
    else {
      val u = this.unparser
      if (u.isEmpty) Maybe.Nope
      else Maybe(u)
    }
  }
}
