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

package org.apache.daffodil.core.grammar

import org.apache.daffodil.core.dsom.Term
import org.apache.daffodil.core.grammar.primitives.MandatoryTextAlignment
import org.apache.daffodil.core.runtime1.TermRuntime1Mixin

/////////////////////////////////////////////////////////////////
// Common to all Terms (Elements and ModelGroups)
/////////////////////////////////////////////////////////////////

trait TermGrammarMixin extends AlignedMixin with BitOrderMixin with TermRuntime1Mixin {
  self: Term =>

  override protected final def grammarContext = this

  def termContentBody: Gram

  private lazy val newVars = this.newVariableInstanceStatements

  private lazy val newVarStarts = newVars.map { _.gram(self) }
  private lazy val newVarEnds = newVars.map { _.endGram(self) }

  protected lazy val hasEncoding = optionEncodingRaw.isDefined

  // TODO: replace dfdlScopeBegin and dfdlScopeEnd with a single Combinator.
  protected final lazy val dfdlScopeBegin = prod("dfdlScopeBegin", newVarStarts.length > 0) {
    newVarStarts.fold(mt) { _ ~ _ }
  }

  protected final lazy val dfdlScopeEnd = prod("dfdlScopeEnd", newVarEnds.length > 0) {
    newVarEnds.fold(mt) { _ ~ _ }
  }

  /**
   * Mandatory text alignment or mta
   *
   * mta can only apply to things with encodings. No encoding, no MTA.
   *
   * In addition, it has to be textual data. Just because there's an encoding
   * in the property environment shouldn't get you an MTA region. It has
   * to be textual.
   */
  protected final lazy val mtaBase = prod("mandatoryTextAlignment", hasEncoding) {
    MandatoryTextAlignment(this, knownEncodingAlignmentInBits, false)
  }

  /**
   * Mandatory text alignment for delimiters
   */
  final lazy val delimMTA = prod(
    "delimMTA", {
      hasDelimiters
    }
  ) {
    // This is different from mtaBase because it passes in 'true' for the
    // last parameter to signify that it is MTA for a delimiter. mtaBase
    // passes in 'false'
    MandatoryTextAlignment(this, knownEncodingAlignmentInBits, true)
  }

}
