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

import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.dsom.Term
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.processors.parsers.AlignmentFillParser
import org.apache.daffodil.processors.parsers.MandatoryTextAlignmentParser
import org.apache.daffodil.processors.parsers.Parser
import org.apache.daffodil.processors.parsers.SkipRegionParser
import org.apache.daffodil.processors.unparsers.AlignmentFillUnparser
import org.apache.daffodil.processors.unparsers.MandatoryTextAlignmentUnparser
import org.apache.daffodil.processors.unparsers.SkipRegionUnparser
import org.apache.daffodil.processors.unparsers.Unparser
import org.apache.daffodil.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.dsom.TunableLimitExceededError

abstract class SkipRegion(e: Term, skipLengthInBits: Int, propName: String) extends Terminal(e, skipLengthInBits > 0) {

  if (skipLengthInBits > e.tunable.maxSkipLengthInBytes * 8) {
    throw new TunableLimitExceededError(
      e.schemaFileLocation,
      "Property %s %s(bits) is larger than limit %s(bits).", propName, skipLengthInBits, e.tunable.maxSkipLengthInBytes * 8)
  }

  final lazy val parser: Parser = new SkipRegionParser(skipLengthInBits, e.termRuntimeData)
  final lazy val unparser: Unparser = new SkipRegionUnparser(skipLengthInBits, e.termRuntimeData)
}

case class LeadingSkipRegion(e: Term) extends SkipRegion(e, e.leadingSkipInBits, "leadingSkip")

case class TrailingSkipRegion(e: Term) extends SkipRegion(e, e.trailingSkipInBits, "trailingSkip") {

  e match {
    case eb: ElementBase => {
      e.schemaDefinitionWhen(
        e.trailingSkip > 0 && eb.lengthKind == LengthKind.Delimited && !e.hasTerminator,
        "Property terminator must be defined when trailingSkip > 0 and lengthKind='delimited'")
    }
    case _ => // ok
  }
}

case class AlignmentFill(e: Term) extends Terminal(e, !e.isKnownToBeAligned) {

  private lazy val alignment = e.alignmentValueInBits // must be lazy, else guard can't "leave out" this term and then checks that are irrelevant will be done.

  lazy val parser: Parser = new AlignmentFillParser(alignment, e.termRuntimeData)
  lazy val unparser: Unparser = new AlignmentFillUnparser(alignment, e.termRuntimeData)
}

case class MandatoryTextAlignment(
  e: Term,
  alignmentInBits: Int,
  forDelimiter: Boolean)
  extends Terminal(
    e,
    if (forDelimiter)
      !e.isDelimiterKnownToBeTextAligned
    else
      !e.isKnownToBeTextAligned) {
  Assert.invariant(alignmentInBits > 0)

  lazy val parser: Parser = new MandatoryTextAlignmentParser(alignmentInBits, e.termRuntimeData)
  lazy val unparser: Unparser = new MandatoryTextAlignmentUnparser(alignmentInBits, e.termRuntimeData)
}
