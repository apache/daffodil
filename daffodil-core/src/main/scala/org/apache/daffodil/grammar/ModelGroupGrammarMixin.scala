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

import org.apache.daffodil.schema.annotation.props.gen._
import org.apache.daffodil.dsom.InitiatedTerminatedMixin
import org.apache.daffodil.dsom.ModelGroup
import org.apache.daffodil.grammar.primitives.TrailingSkipRegion
import org.apache.daffodil.grammar.primitives.LeadingSkipRegion
import org.apache.daffodil.grammar.primitives.AlignmentFill
import org.apache.daffodil.grammar.primitives.DelimiterStackCombinatorSequence
import org.apache.daffodil.grammar.primitives.DelimiterStackCombinatorChoice
import org.apache.daffodil.dsom.SequenceTermBase
import org.apache.daffodil.dsom.ChoiceTermBase

trait ModelGroupGrammarMixin
  extends InitiatedTerminatedMixin
  with AlignedMixin
  with HasStatementsGrammarMixin
  with GroupCommonAGMixin { self: ModelGroup =>

  private lazy val groupLeftFraming = prod("groupLeftFraming") {
    LeadingSkipRegion(this) ~ AlignmentFill(this)
  }

  private lazy val groupRightFraming = prod("groupRightFraming") { TrailingSkipRegion(this) }

  // I believe we can have the same grammar rules whether we're directly inside a complex type, or
  // we're nested inside another group as a term.
  final lazy val asChildOfComplexType = termContentBody

  final override lazy val termContentBody = prod("termContentBody") {
    dfdlStatementEvaluations ~ groupLeftFraming ~ _content ~ groupRightFraming
  }

  private lazy val _content = prod("_content") {
    val finalContent =
      if (hasDelimiters ||
        enclosingTerm.map(_.hasDelimiters).getOrElse(false) //
        // The above refernce to the delimiters of the enclosing term,
        // has to do with the way our delim stack works.
        // Even if this model group doesn't have delimiters,
        // if the enclosing term did have delimiters, then we still need to
        // add a delimiter stack parser for this term so that it will modify
        // the stack to signify that existing delimiters are now remote and
        // there are no local delimiters.
        //
        ) {
        val content = initiatorRegion ~ groupContent ~ terminatorRegion
        self match {
          case c: ChoiceTermBase => DelimiterStackCombinatorChoice(c, content)
          case s: SequenceTermBase => DelimiterStackCombinatorSequence(s, content)
        }
      } else { groupContent }

    finalContent
  }

  protected def groupContent: Gram
}
