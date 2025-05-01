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

package org.apache.daffodil.core.grammar.primitives

import java.util.regex.PatternSyntaxException

import org.apache.daffodil.core.dsom.ElementBase
import org.apache.daffodil.core.grammar.Gram
import org.apache.daffodil.core.grammar.Terminal
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType
import org.apache.daffodil.runtime1.processors.parsers._
import org.apache.daffodil.runtime1.processors.unparsers._
import org.apache.daffodil.unparsers.runtime1._

abstract class SpecifiedLengthCombinatorBase(val e: ElementBase, eGramArg: => Gram)
  extends Terminal(e, true) {

  lazy val eGram = eGramArg // once only

  final protected lazy val eParser: Parser = {
    val p = eGram.parser
    p
  }

  lazy val eUnparser = {
    val u = eGram.unparser
    u
  }

  def kind: String

  def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..."
    else
      "<SpecifiedLengthCombinator_" + kind + ">" +
        eParser.toBriefXML(depthLimit - 1) +
        "</SpecifiedLengthCombinator_" + kind + ">"
  }

}

class SpecifiedLengthPattern(e: ElementBase, eGram: => Gram)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "Pattern"

  lazy val pattern =
    try {
      e.lengthPattern.r.pattern // imagine a really big expensive pattern to compile.
    } catch {
      case x: PatternSyntaxException =>
        e.SDE(x)
    }

  if (!e.encodingInfo.isScannable && !(e.isSimpleType && e.primType == PrimType.HexBinary)) {
    // lengthKind="pattern" requires scanability. However, xs:hexBinary types
    // are not scannable, but we do allow pattern lengths if the encoding is
    // ISO-8859-1. The ISO-8859-1 check is done elsewhere, so to allow pattern
    // lengths, either this must be scannable or the type must be xs:hexBinary.
    // Anything else is an error.
    e.SDE(
      "Element %s does not meet the requirements of Pattern-Based lengths and Scanability.\nThe element and its children must be representation='text' and share the same encoding.",
      e.diagnosticDebugName
    )
  }

  override lazy val parser: Parser =
    new SpecifiedLengthPatternParser(eParser, e.elementRuntimeData, pattern)

  // When unparsing, the pattern is not used to calculate a length, so just
  // skip that parser and go straight to unparsing the string in the eUnparser
  override lazy val unparser: Unparser = {
    pattern
    // force pattern just so we detect regex syntax errors even though
    // we don't use the pattern when unparsing.
    eUnparser
  }
}

trait SpecifiedLengthExplicitImplicitUnparserMixin {

  def e: ElementBase
  def eUnparser: Unparser

  lazy val unparser: Unparser = {
    val u = eUnparser
    if (u.isEmpty) u
    else
      new SpecifiedLengthExplicitImplicitUnparser(
        u,
        e.elementRuntimeData,
        e.unparseTargetLengthInBitsEv
      )
  }
}

class SpecifiedLengthExplicit(e: ElementBase, eGram: => Gram, bitsMultiplier: Int)
  extends SpecifiedLengthCombinatorBase(e, eGram)
  with SpecifiedLengthExplicitImplicitUnparserMixin {

  Assert.usage(bitsMultiplier > 0)

  lazy val kind = "Explicit_" + e.lengthUnits.toString

  lazy val parser: Parser = {
    if (eParser.isEmpty) eParser
    else
      new SpecifiedLengthExplicitParser(
        eParser,
        e.elementRuntimeData,
        e.lengthEv,
        bitsMultiplier
      )
  }

}

class SpecifiedLengthImplicit(e: ElementBase, eGram: => Gram, nBits: Long)
  extends SpecifiedLengthCombinatorBase(e, eGram)
  with SpecifiedLengthExplicitImplicitUnparserMixin {

  lazy val kind = "Implicit_" + e.lengthUnits.toString

  lazy val toBits = 1

  lazy val parser: Parser =
    new SpecifiedLengthImplicitParser(eParser, e.elementRuntimeData, nBits)

}

class SpecifiedLengthPrefixed(e: ElementBase, eGram: => Gram, bitsMultiplier: Int)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  Assert.usage(bitsMultiplier > 0)

  lazy val kind = "Prefixed_" + e.lengthUnits.toString

  private lazy val erd = e.elementRuntimeData
  private lazy val plerd = e.prefixedLengthElementDecl.elementRuntimeData
  private lazy val pladj = e.prefixedLengthAdjustmentInUnits

  lazy val parser: Parser = new SpecifiedLengthPrefixedParser(
    eParser,
    erd,
    e.prefixedLengthBody.parser,
    plerd,
    e.lengthUnits,
    pladj
  )

  lazy val unparser: Unparser = {
    if (
      e.lengthUnits == LengthUnits.Characters &&
      e.isComplexType &&
      !e.encodingInfo.knownEncodingIsFixedWidth
    )
      e.subsetError(
        "Unparsing with dfdl:lengthUnits 'characters' for complex types requires a fixed-width known (constant) encoding."
      )
    new SpecifiedLengthPrefixedUnparser(
      eUnparser,
      erd,
      e.prefixedLengthBody.unparser,
      plerd,
      e.lengthUnits,
      pladj
    )
  }
}

class SpecifiedLengthExplicitCharacters(e: ElementBase, eGram: => Gram)
  extends SpecifiedLengthCombinatorBase(e, eGram)
  with SpecifiedLengthExplicitImplicitUnparserMixin {

  val kind = "ExplicitCharacters"

  lazy val parser: Parser =
    new SpecifiedLengthExplicitCharactersParser(eParser, e.elementRuntimeData, e.lengthEv)
}

class SpecifiedLengthImplicitCharacters(e: ElementBase, eGram: => Gram, nChars: Long)
  extends SpecifiedLengthCombinatorBase(e, eGram)
  with SpecifiedLengthExplicitImplicitUnparserMixin {

  val kind = "ImplicitCharacters"

  lazy val parser: Parser =
    new SpecifiedLengthImplicitCharactersParser(eParser, e.elementRuntimeData, nChars)
}

class SpecifiedLengthPrefixedCharacters(e: ElementBase, eGram: => Gram)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "ExplicitCharacters"

  lazy val parser: Parser = new SpecifiedLengthPrefixedCharactersParser(
    eParser,
    e.elementRuntimeData,
    e.prefixedLengthBody.parser,
    e.prefixedLengthElementDecl.elementRuntimeData,
    e.prefixedLengthAdjustmentInUnits
  )

  lazy val unparser: Unparser =
    e.subsetError(
      """Unparsing with dfdl:lengthKind='prefixed' and dfdl:lengthUnits='characters' and
        |a non-constant or variable-width encoding is not supported.""".stripMargin
    )
}
