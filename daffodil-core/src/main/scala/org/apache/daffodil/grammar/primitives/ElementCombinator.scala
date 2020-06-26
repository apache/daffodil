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
import org.apache.daffodil.equality.TypeEqual
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.grammar.Gram
import org.apache.daffodil.grammar.NamedGram
import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.processors.parsers.ElementParser
import org.apache.daffodil.processors.parsers.ElementParserNoRep
import org.apache.daffodil.processors.parsers.Parser
import org.apache.daffodil.processors.parsers.CaptureEndOfContentLengthParser
import org.apache.daffodil.processors.parsers.CaptureEndOfValueLengthParser
import org.apache.daffodil.processors.parsers.CaptureStartOfContentLengthParser
import org.apache.daffodil.processors.parsers.CaptureStartOfValueLengthParser
import org.apache.daffodil.processors.unparsers.CaptureEndOfContentLengthUnparser
import org.apache.daffodil.processors.unparsers.CaptureEndOfValueLengthUnparser
import org.apache.daffodil.processors.unparsers.CaptureStartOfContentLengthUnparser
import org.apache.daffodil.processors.unparsers.CaptureStartOfValueLengthUnparser
import org.apache.daffodil.processors.unparsers.ElementOVCSpecifiedLengthUnparser
import org.apache.daffodil.processors.unparsers.ElementOVCUnspecifiedLengthUnparser
import org.apache.daffodil.processors.unparsers.ElementSpecifiedLengthUnparser
import org.apache.daffodil.processors.unparsers.ElementUnparserNoRep
import org.apache.daffodil.processors.unparsers.ElementUnspecifiedLengthUnparser
import org.apache.daffodil.processors.unparsers.ElementUnusedUnparser
import org.apache.daffodil.processors.unparsers.LeftCenteredPaddingUnparser
import org.apache.daffodil.processors.unparsers.SimpleTypeRetryUnparser
import org.apache.daffodil.processors.unparsers.OnlyPaddingUnparser
import org.apache.daffodil.processors.unparsers.RightCenteredPaddingUnparser
import org.apache.daffodil.processors.unparsers.RightFillUnparser
import org.apache.daffodil.processors.unparsers.Unparser
import org.apache.daffodil.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.schema.annotation.props.gen.Representation
import org.apache.daffodil.schema.annotation.props.gen.TestKind
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.processors.parsers.NadaParser
import org.apache.daffodil.processors.unparsers.NadaUnparser
import org.apache.daffodil.grammar.EmptyGram

/**
 * This uber combinator exists because we (currently) do quite different things
 * for parsing and unparsing.
 *
 * It lets us introduce the new unparser capabilities for the situations where
 * they are truly necessary, and keep using the older style stuff for the
 * situations where it works already.
 *
 * Ultimately, some big refactoring is needed here though, or this is going to
 * get very complicated to reason about, as if it isn't already :-), well this
 * is going to make it worse.
 */
class ElementCombinator(
  context: ElementBase,
  eBeforeContent: Gram,
  eValue: Gram,
  eAfterValue: Gram,
  repTypeElementGram: Gram = EmptyGram
  )
  extends NamedGram(context)
  with Padded {

  override def toString = subComb.toString()

  private lazy val subComb = new ElementParseAndUnspecifiedLength(context, eBeforeContent,
    eValue, eAfterValue, repTypeElementGram)

  override lazy val parser: Parser = {
    //
    // This sub combinator is exactly what we've done for a while
    // for parsing
    //

    subComb.parser
  }

  private lazy val uSetVars = context.setVariableStatements.map(_.gram(context).unparser).toArray

  private lazy val eBeforeUnparser: Maybe[Unparser] =
    if (eBeforeContent.isEmpty) Maybe.Nope
    else Maybe(eBeforeContent.unparser)

  private lazy val eUnparser: Maybe[Unparser] =
    if (eValue.isEmpty) Maybe.Nope
    else Maybe(eValue.unparser)

  private lazy val eAfterUnparser: Maybe[Unparser] =
    if (eAfterValue.isEmpty) Maybe.Nope
    else Maybe(eAfterValue.unparser)

  private lazy val eReptypeUnparser: Maybe[Unparser] = repTypeElementGram.maybeUnparser
    
  override lazy val unparser: Unparser = {
    if (context.isOutputValueCalc) {
      new ElementOVCSpecifiedLengthUnparser(
        context.erd,
        context.maybeUnparseTargetLengthInBitsEv,
        uSetVars,
        eBeforeUnparser,
        eUnparser,
        eAfterUnparser)
    } else if ((context.lengthKind _eq_ LengthKind.Explicit) ||
      (context.isSimpleType &&
        (context.lengthKind _eq_ LengthKind.Implicit) &&
        (context.impliedRepresentation _eq_ Representation.Text))) {

      new ElementSpecifiedLengthUnparser(
        context.erd,
        context.maybeUnparseTargetLengthInBitsEv,
        uSetVars,
        eBeforeUnparser,
        eUnparser,
        eAfterUnparser,
        eReptypeUnparser
      )
    } else {
      subComb.unparser
    }
  }

}

case class ElementUnused(ctxt: ElementBase)
  extends Terminal(ctxt, ctxt.shouldAddFill ||
    ctxt.shouldCheckExcessLength) {
  override def parser = new NadaParser(ctxt.erd)

  override lazy val unparser: Unparser = new ElementUnusedUnparser(
    ctxt.erd,
    ctxt.maybeUnparseTargetLengthInBitsEv.get,
    ctxt.maybeLengthEv,
    ctxt.maybeCharsetEv,
    ctxt.maybeLiteralNilEv)
}

case class OnlyPadding(ctxt: ElementBase)
  extends Terminal(ctxt, ctxt.shouldAddPadding)
  with Padded {

  override def parser = new NadaParser(ctxt.erd)

  override lazy val unparser: Unparser = {
    Assert.invariant(ctxt.maybeUnparseMinOrTargetLengthInBitsEv.isDefined)
    val mmtlev = ctxt.maybeUnparseMinOrTargetLengthInBitsEv.get
    new OnlyPaddingUnparser(
      ctxt.erd,
      mmtlev,
      ctxt.maybeLengthEv,
      ctxt.maybeCharsetEv,
      ctxt.maybeLiteralNilEv,
      unparsingPadChar)
  }
}

case class RightCenteredPadding(ctxt: ElementBase)
  extends Terminal(ctxt, ctxt.shouldAddPadding)
  with Padded {
  override def parser = new NadaParser(ctxt.erd)

  override lazy val unparser: Unparser =
    new RightCenteredPaddingUnparser(
      ctxt.erd,
      ctxt.maybeUnparseMinOrTargetLengthInBitsEv.get,
      ctxt.maybeLengthEv,
      ctxt.maybeCharsetEv,
      ctxt.maybeLiteralNilEv,
      unparsingPadChar)
}

case class LeftCenteredPadding(ctxt: ElementBase)
  extends Terminal(ctxt, ctxt.shouldAddPadding)
  with Padded {
  override def parser = new NadaParser(ctxt.erd)

  override lazy val unparser: Unparser =
    new LeftCenteredPaddingUnparser(
      ctxt.erd,
      ctxt.maybeUnparseMinOrTargetLengthInBitsEv.get,
      ctxt.maybeLengthEv,
      ctxt.maybeCharsetEv,
      ctxt.maybeLiteralNilEv,
      unparsingPadChar)
}

case class RightFill(ctxt: ElementBase)
  extends Terminal(ctxt, ctxt.shouldAddFill ||
    ctxt.shouldCheckExcessLength)
  with Padded {
  override def parser = new NadaParser(ctxt.erd)

  override lazy val unparser: Unparser = new RightFillUnparser(
    ctxt.erd,
    ctxt.maybeUnparseTargetLengthInBitsEv.get,
    ctxt.maybeLengthEv,
    ctxt.maybeCharsetEv,
    ctxt.maybeLiteralNilEv,
    unparsingPadChar)
}

case class SimpleTypeRetry(ctxt: ElementBase, v: Gram)
  extends Terminal(ctxt, true) {
  override def parser = v.parser

  override def unparser = new SimpleTypeRetryUnparser(
    ctxt.erd,
    ctxt.maybeUnparseTargetLengthInBitsEv, v.unparser)
}

case class CaptureContentLengthStart(ctxt: ElementBase)
  extends Terminal(ctxt, true) {
  override def parser =
    if (ctxt.shouldCaptureParseContentLength)
      new CaptureStartOfContentLengthParser(ctxt.erd)
    else
      new NadaParser(ctxt.erd)

  override lazy val unparser: Unparser =
    if (ctxt.shouldCaptureUnparseContentLength)
      new CaptureStartOfContentLengthUnparser(ctxt.erd)
    else
      new NadaUnparser(ctxt.erd)
}

case class CaptureContentLengthEnd(ctxt: ElementBase)
  extends Terminal(ctxt, true) {
  override def parser =
    if (ctxt.shouldCaptureParseContentLength)
      new CaptureEndOfContentLengthParser(ctxt.erd)
    else
      new NadaParser(ctxt.erd)

  override lazy val unparser: Unparser =
    if (ctxt.shouldCaptureUnparseContentLength)
      new CaptureEndOfContentLengthUnparser(ctxt.erd, ctxt.maybeFixedLengthInBits)
    else
      new NadaUnparser(ctxt.erd)
}

case class CaptureValueLengthStart(ctxt: ElementBase)
  extends Terminal(ctxt, true) {
  override def parser =
    if (ctxt.shouldCaptureParseValueLength)
      new CaptureStartOfValueLengthParser(ctxt.erd)
    else
      new NadaParser(ctxt.erd)

  override lazy val unparser: Unparser =
    if (ctxt.shouldCaptureUnparseValueLength)
      new CaptureStartOfValueLengthUnparser(ctxt.erd)
    else
      new NadaUnparser(ctxt.erd)
}

case class CaptureValueLengthEnd(ctxt: ElementBase)
  extends Terminal(ctxt, true) {
  override def parser =
    if (ctxt.shouldCaptureParseValueLength)
      new CaptureEndOfValueLengthParser(ctxt.erd)
    else
      new NadaParser(ctxt.erd)

  override lazy val unparser: Unparser =
    if (ctxt.shouldCaptureUnparseValueLength)
      new CaptureEndOfValueLengthUnparser(ctxt.erd)
    else
      new NadaUnparser(ctxt.erd)
}


class ElementParseAndUnspecifiedLength(context: ElementBase, eBeforeGram: Gram, eGram: Gram, eAfterGram: Gram, repTypeElementGram: Gram)
  extends ElementCombinatorBase(context, eBeforeGram, eGram, eAfterGram, repTypeElementGram) {

  lazy val parser: Parser =
    if (context.isRepresented)
      new ElementParser(
        context.erd,
        context.name,
        patDiscrim,
        patAssert,
        pSetVar,
        testDiscrim,
        testAssert,
        eBeforeParser,
        eParser,
        eAfterParser,
        eRepTypeParser
      )
    else
      new ElementParserNoRep(
        context.erd,
        context.name,
        patDiscrim,
        patAssert,
        pSetVar,
        testDiscrim,
        testAssert,
        eBeforeParser,
        eParser,
        eAfterParser)

  override lazy val unparser: Unparser = {
    if (context.isRepresented) {
      if (context.isOutputValueCalc) {
        new ElementOVCUnspecifiedLengthUnparser(context.erd, uSetVar, eBeforeUnparser, eUnparser, eAfterUnparser)
      } else {
        new ElementUnspecifiedLengthUnparser(context.erd, uSetVar, eBeforeUnparser, eUnparser, eAfterUnparser, eRepTypeUnparser)
      }
    } else {
      // dfdl:inputValueCalc case.
      // This unparser will assume the events are in the event stream, having been inferred and put
      // in place by the next element resolver.
      new ElementUnparserNoRep(context.erd, uSetVar)
    }
  }
}

abstract class ElementCombinatorBase(context: ElementBase, eGramBefore: Gram, eGram: Gram, eGramAfter: Gram, repTypeElementGram:Gram)
  extends NamedGram(context) {

  override def toString() = "<element name='" + name + "'>" + eGram.toString() + "</element>"

  // The order of things matters in some cases, so to be consistent we'll always use the
  // same order even when it doesn't matter

  // The order of evaluation of statements is:
  // - pattern discriminators
  // - pattern asserts
  // - the parsing of the element itself
  // - setVariables
  // - test discriminators (must be attempted even if the parsing of element or setVariable statements fail)
  // - test asserts

  // requiredEvaluationsAlways(patDiscrim, patAssert, eGram, setVar, testDiscrim, testAssert)
  // Note: above not needed as these are ALWAYS evaluated below.

  lazy val patDiscrim = {
    val pd = context.discriminatorStatements.filter(_.testKind == TestKind.Pattern)
    Assert.invariant(pd.size <= 1)
    if (pd.size == 0) {
      Maybe.Nope
    } else {
      pd(0).gram(context).maybeParser
    }
  }
  lazy val patAssert = context.assertStatements.filter(_.testKind == TestKind.Pattern).map(_.gram(context).parser).toArray
  lazy val pSetVar = context.setVariableStatements.map(_.gram(context).parser).toArray
  lazy val testDiscrim = {
    val td = context.discriminatorStatements.filter(_.testKind == TestKind.Expression)
    Assert.invariant(td.size <= 1)
    if (td.size == 0) {
      Maybe.Nope
    } else {
      td(0).gram(context).maybeParser
    }
  }
  lazy val testAssert = context.assertStatements.filter(_.testKind == TestKind.Expression).map(_.gram(context).parser).toArray

  lazy val eBeforeParser: Maybe[Parser] = eGramBefore.maybeParser

  lazy val eParser: Maybe[Parser] = eGram.maybeParser

  lazy val eAfterParser: Maybe[Parser] = eGramAfter.maybeParser
  
  lazy val eRepTypeParser: Maybe[Parser] = repTypeElementGram.maybeParser

  def parser: Parser

  lazy val uSetVar = context.setVariableStatements.map(_.gram(context).unparser).toArray

  lazy val eBeforeUnparser: Maybe[Unparser] = eGramBefore.maybeUnparser

  lazy val eUnparser: Maybe[Unparser] = eGram.maybeUnparser

  lazy val eAfterUnparser: Maybe[Unparser] = eGramAfter.maybeUnparser
  
  lazy val eRepTypeUnparser: Maybe[Unparser] = repTypeElementGram.maybeUnparser

  def unparser: Unparser

}
