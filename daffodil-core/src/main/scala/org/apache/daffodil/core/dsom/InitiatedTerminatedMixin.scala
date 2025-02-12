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

package org.apache.daffodil.core.dsom

import org.apache.daffodil.core.grammar.GrammarMixin
import org.apache.daffodil.core.grammar.primitives.InitiatedContent
import org.apache.daffodil.core.grammar.primitives.Initiator
import org.apache.daffodil.core.grammar.primitives.Terminator
import org.apache.daffodil.lib.schema.annotation.props.gen._

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

  /**
   * True if the term has an initiator expressed on it.
   *
   * Do not confuse with the concept of the delimiter being able to match or not match zero-length data.
   * Whether the representation of a term in the data stream "has an initiator", as in the initiator
   * occupies a non-zero number of bits in the data stream, is an entirely different question.
   */
  lazy val hasInitiator = {
    val hasOne = !initiatorExpr.isConstantEmptyString
    hasOne
  }

  /**
   * True if the term's initiator cannot match zero-length data. This answers the entirely different
   * question of whether the initiator occupies a non-zero number of bits in the data stream.
   */
  lazy val hasNonZeroLengthInitiator = {
    val hasOne = !initiatorExpr.isKnownCanMatchEmptyString
    hasOne
  }

  /**
   * True if the term is inside an immediately enclosing model group which has the initiatedContent
   * property set to "yes". This tells us whether we need to verify that a runtime expression defining
   * the initiator matches a non-zero number of bits in the data stream.
   */
  lazy val mustMatchNonZeroData = parentSaysInitiatedContent

  /**
   * True if the term has a terminator expressed on it.
   *
   * Do not confuse with the concept of the delimiter being able to match or not match zero-length data.
   * Whether the representation of a term in the data stream "has a terminator", as in the terminator
   * occupies a non-zero number of bits, is an entirely different question.
   */
  lazy val hasTerminator = {
    val hasOne = !terminatorExpr.isConstantEmptyString
    hasOne
  }

  private lazy val isInitiatedContentChoice: Boolean = {
    immediatelyEnclosingModelGroup
      .map {
        case c: ChoiceTermBase => c.initiatedContent == YesNo.Yes
        case _ => false
      }
      .getOrElse(false)
  }

  private lazy val shouldUseInitiatorDiscriminator: Boolean = {
    parentSaysInitiatedContent &&
    immediatelyEnclosingGroupDef
      .map {
        case c: ChoiceTermBase => true
        case s: SequenceTermBase =>
          (isArray || isOptional) &&
          isVariableOccurrences
      }
      .getOrElse(false)
  }

  private lazy val initiatorDiscriminator =
    prod("initiatorDiscriminator", shouldUseInitiatorDiscriminator) {
      this match {
        case eb: ElementBase => {
          if (eb.minOccurs < 1 && isInitiatedContentChoice) {
            SDE(
              "The minOccurs attribute should not be zero when dfdl:initiatedContent is 'yes'."
            )
          }
        }
        case _ => // ok
      }
      InitiatedContent(immediatelyEnclosingModelGroup.get, this)
    }

  lazy val initiatorRegion = prod("initiatorRegion", hasInitiator) {
    initiatorItself ~ initiatorDiscriminator
  }
  private lazy val initiatorItself = delimMTA ~ Initiator(this)

  lazy val terminatorRegion = prod("terminatorRegion", hasTerminator) {
    delimMTA ~ Terminator(this)
  }

  /**
   * True if this term has initiator, terminator, or separator that are either statically
   * present, or there is an expression. (Such expressions are not allowed to evaluate to "" - you
   * can't turn off a delimiter by providing "" at runtime. Minimum length is 1 for these at runtime.
   * <p>
   * Override in SequenceTermBase to also check for separator.
   */
  lazy val hasDelimiters = hasInitiator || hasTerminator

}
