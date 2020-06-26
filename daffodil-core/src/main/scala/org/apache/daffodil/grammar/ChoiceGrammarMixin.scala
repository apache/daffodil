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

package org.apache.daffodil.grammar

import org.apache.daffodil.dsom.ChoiceBranchImpliedSequence
import org.apache.daffodil.dsom.ChoiceTermBase
import org.apache.daffodil.grammar.primitives.ChoiceCombinator
import org.apache.daffodil.runtime1.ChoiceTermRuntime1Mixin

trait ChoiceGrammarMixin
  extends GrammarMixin
  with ChoiceTermRuntime1Mixin { self: ChoiceTermBase =>

  override lazy val groupContentDef = prod("choiceContent") {
    ChoiceCombinator(this, alternatives)
  }

  /**
   * Establish the invariant is that if a direct child member is an array element,
   * the child will have been encapsulated as a sequence, so that arrays always
   * live within sequences.
   */
  final protected lazy val alternatives: Seq[Gram] = {
    groupMembers.map { t =>
      if (!t.isScalar) {
        /**
         * If this choice branch is a non-scalar, then we need to encapsulate
         * it with a ChoiceBranchImpliedSequence, which is a kind of Sequence
         * base. When compiling this this choice branch, Daffodil can then
         * depend on the invariant that every recurring element is contained
         * inside a sequence, and that sequence describes everything about how
         * that elements occurrences are separated.
         */
        val cbis = new ChoiceBranchImpliedSequence(t)
        cbis.termContentBody
      } else {
        t.termContentBody
      }
    }
  }
}
