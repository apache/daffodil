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
import org.apache.daffodil.grammar.primitives._
import org.apache.daffodil.dsom.SequenceTermBase

trait SequenceGrammarMixin extends GrammarMixin { self: SequenceTermBase =>

  final override lazy val groupContent = prod("groupContent") {
    if (isLayered) layeredSequenceContent
    else {
      self.sequenceKind match {
        case SequenceKind.Ordered => orderedSequenceContent
        case SequenceKind.Unordered => subsetError("Unordered sequences are not supported.") // unorderedSequenceContent
      }
    }
  }

  private lazy val layeredSequenceContent = {
    schemaDefinitionUnless(groupMembers.length == 1, "Layered sequence can have only 1 child term. %s were found: %s", groupMembers.length,
      groupMembers.mkString(", "))
    val term = groupMembers(0)
    schemaDefinitionWhen(term.isArray, "Layered sequence body cannot be an array.")
    val termGram = term.termContentBody
    LayeredSequence(this, termGram)
  }

  private lazy val orderedSequenceContent = prod("sequenceContent") {
    (self.sequenceKind, hasSeparator) match {
      case (SequenceKind.Ordered, true) => orderedSeparatedSequence
      case (SequenceKind.Ordered, false) => orderedUnseparatedSequence
      case (SequenceKind.Unordered, _) => subsetError("Unordered sequences are not supported.") // unorderedSequenceContent
    }
  }

  private lazy val orderedSeparatedSequence = {
    val gm = groupMembers
    val res = OrderedSeparatedSequence(this, gm)
    res
  }

  private lazy val orderedUnseparatedSequence =
    OrderedUnseparatedSequence(this, groupMembers)

  /**
   * These are static properties even though the delimiters can have runtime-computed values.
   * The existence of an expression to compute a delimiter is assumed to imply a non-zero-length, aka a real delimiter.
   */
  final lazy val hasPrefixSep = sepExpr(SeparatorPosition.Prefix)

  final lazy val hasInfixSep = sepExpr(SeparatorPosition.Infix)

  final lazy val hasPostfixSep = sepExpr(SeparatorPosition.Postfix)

  // note use of pass by value. We don't want to even need the SeparatorPosition property unless there is a separator.
  private def sepExpr(pos: => SeparatorPosition): Boolean = {
    if (hasSeparator) if (separatorPosition eq pos) true else false
    else false
  }

  final lazy val hasSeparator = separatorParseEv.isKnownNonEmpty

  lazy val sequenceSeparator = prod("separator", hasSeparator) {
    //
    // TODO: (JIRA DFDL-1400) The separators may be in a different encoding than the terms
    // that they separate.
    //
    // So we must allow for a change of encoding (which may also imply a change
    // of bit order)
    //
    // However, this isn't the same as just plopping down a bitOrderChange ~ encodingChange, since
    // those examine prior peer, and what we want to scrutinize is the prior term being separated.
    //
    delimMTA ~ SequenceSeparator(this)
  }
}

