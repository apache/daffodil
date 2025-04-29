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

import org.apache.daffodil.core.dsom.{
  ChoiceTermBase,
  GroupRef,
  InitiatedTerminatedMixin,
  ModelGroup,
  SequenceTermBase
}
import org.apache.daffodil.core.grammar.primitives.AlignmentFill
import org.apache.daffodil.core.grammar.primitives.DelimiterStackCombinatorChoice
import org.apache.daffodil.core.grammar.primitives.DelimiterStackCombinatorSequence
import org.apache.daffodil.core.grammar.primitives.HiddenGroupCombinator
import org.apache.daffodil.core.grammar.primitives.LeadingSkipRegion
import org.apache.daffodil.core.grammar.primitives.TrailingSkipRegion
import org.apache.daffodil.core.runtime1.ModelGroupRuntime1Mixin
import org.apache.daffodil.lib.schema.annotation.props.gen._

trait ModelGroupGrammarMixin
  extends InitiatedTerminatedMixin
  with AlignedMixin
  with HasStatementsGrammarMixin
  with GroupCommonAGMixin
  with ModelGroupRuntime1Mixin { self: ModelGroup =>

  private lazy val groupLeftFraming = prod("groupLeftFraming") {
    LeadingSkipRegion(this) ~ AlignmentFill(this)
  }

  private lazy val groupRightFraming = prod("groupRightFraming") { TrailingSkipRegion(this) }

  final override lazy val termContentBody = prod("termContentBody") {
    // See 9.5 Evaluation Order for Statement Annotations
    dfdlPatternStatementEvaluations ~ // Assert and Discriminator statements with testKind="pattern"
      dfdlScopeBegin ~ // newVariableInstance
      dfdlLowPriorityStatementEvaluations ~ // setVariable and the rest of the Assert and Discriminator statements
      groupLeftFraming ~ groupContentWithInitiatorTerminator ~ groupRightFraming ~ dfdlScopeEnd
  }

  private lazy val groupContentWithInitiatorTerminator =
    prod("groupContentWithInitiatorTerminator") {
      val finalContent = {
        if (
          hasDelimiters ||
          immediatelyEnclosingModelGroup.map(_.hasDelimiters).getOrElse(false) //
          // The above reference to the delimiters of the enclosing term,
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
        } else {
          groupContent
        }
      }

      this match {
        case gr: GroupRef if gr.isHidden => new HiddenGroupCombinator(self, finalContent)
        case _ => finalContent
      }
    }

  /**
   * groupContent is shared for all groups across all uses of the group
   * that have the same shareKey. (e.g., by
   * multiple group references if they have common properties).
   * This eliminates redundant computation of the grammar
   * structures and any runtime objects subsequently created.
   *
   * The framing and initiator/terminator are not shared, they surround the shared part.
   *
   * Subclasses define the group content by way of the protected groupContentDef override.
   *
   * It is crucial to efficiency (avoiding redundant computation) that the 2nd argument
   * to getShared is passed by name, not evaluated unless necessary.
   */
  private lazy val groupContent =
    schemaSet.sharedGroupContentsFactory.getShared(shareKey, groupContentDef)

  /**
   * Override to define the actual group content.
   *
   * Must be overridden as a lazy val to avoid redundant computation.
   */
  protected def groupContentDef: Gram
}
