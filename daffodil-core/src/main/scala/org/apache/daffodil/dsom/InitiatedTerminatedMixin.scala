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

package org.apache.daffodil.dsom

import org.apache.daffodil.grammar._
import org.apache.daffodil.schema.annotation.props.gen._
import org.apache.daffodil.grammar.GrammarMixin
import org.apache.daffodil.grammar.primitives.InitiatedContent
import org.apache.daffodil.grammar.primitives.Terminator
import org.apache.daffodil.grammar.primitives.Initiator

trait InitiatedTerminatedMixin
  extends GrammarMixin
  with AnnotatedMixin
  with DelimitedRuntimeValuedPropertiesMixin { self: Term =>

  private lazy val parentSaysInitiatedContent = {
    val parentSays = self.immediatelyEnclosingModelGroup match {
      case Some(s) if (s.initiatedContent == YesNo.Yes) => true
      case _ => false
    }
    parentSays
  }

  lazy val hasInitiator = {
    val hasOne = initiatorExpr.isKnownNonEmpty
    if (parentSaysInitiatedContent)
      schemaDefinitionUnless(hasOne, "Enclosing group has initiatedContent='yes', but initiator is not defined.")
    hasOne
  }

  lazy val hasTerminator = {
    val res = terminatorExpr.isKnownNonEmpty
    res
  }

  lazy val initiatorDiscriminator = prod("initiatorDiscriminator", parentSaysInitiatedContent) { InitiatedContent(this) }

  lazy val initiatorRegion = prod("initiatorRegion", hasInitiator) { initiatorItself ~ initiatorDiscriminator }
  lazy val initiatorItself = delimMTA ~ Initiator(this)

  lazy val terminatorRegion = prod("terminatorRegion", hasTerminator) { delimMTA ~ Terminator(this) }

  /**
   * True if this term has initiator, terminator, or separator that are either statically
   * present, or there is an expression. (Such expressions are not allowed to evaluate to "" - you
   * can't turn off a delimiter by providing "" at runtime. Minimum length is 1 for these at runtime.
   * <p>
   * Override in SequenceTermBase to also check for separator.
   */
  lazy val hasDelimiters = hasInitiator || hasTerminator

}
