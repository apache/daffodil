/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.grammar.primitives

import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.equality.TypeEqual
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.grammar.HasNoUnparser
import edu.illinois.ncsa.daffodil.grammar.NamedGram
import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.processors.parsers.ChoiceElementParser
import edu.illinois.ncsa.daffodil.processors.parsers.ElementParser
import edu.illinois.ncsa.daffodil.processors.parsers.ElementParserNoRep
import edu.illinois.ncsa.daffodil.processors.Parser
import edu.illinois.ncsa.daffodil.processors.parsers.CaptureEndOfContentLengthParser
import edu.illinois.ncsa.daffodil.processors.parsers.CaptureEndOfValueLengthParser
import edu.illinois.ncsa.daffodil.processors.parsers.CaptureStartOfContentLengthParser
import edu.illinois.ncsa.daffodil.processors.parsers.CaptureStartOfValueLengthParser
import edu.illinois.ncsa.daffodil.processors.unparsers.CaptureEndOfContentLengthUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.CaptureEndOfValueLengthUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.CaptureStartOfContentLengthUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.CaptureStartOfValueLengthUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.ElementOVCSpecifiedLengthUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.ElementOVCUnspecifiedLengthUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.ElementSpecifiedLengthUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.ElementUnparserNoRep
import edu.illinois.ncsa.daffodil.processors.unparsers.ElementUnspecifiedLengthUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.ElementUnusedUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.LeftCenteredPaddingUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.NilLiteralCharacterUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.OVCRetryUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.OnlyPaddingUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.RightCenteredPaddingUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.RightFillUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.Unparser
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthKind
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.NilKind
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.Representation
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TestKind
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.processors.parsers.NadaParser
import edu.illinois.ncsa.daffodil.processors.unparsers.NadaUnparser

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
class ElementCombinator(context: ElementBase,
  eBeforeContent: Gram,
  eValue: Gram,
  eAfterValue: Gram)
  extends NamedGram(context)
  with Padded {

  private lazy val subComb = {
    if (context.isParentUnorderedSequence) {
      new ChoiceElementCombinator(context, eBeforeContent,
        eValue, eAfterValue)
    } else {
      new ElementParseAndUnspecifiedLength(context, eBeforeContent,
        eValue, eAfterValue)
    }
  }

  override lazy val parser: Parser = {
    //
    // This sub combinator is exactly what we've done for a while
    // for parsing
    //

    subComb.parser
  }

  private lazy val uSetVars = context.setVariableStatements.map(_.gram.unparser).toArray

  private lazy val eBeforeUnparser: Maybe[Unparser] =
    if (eBeforeContent.isEmpty) Maybe.Nope
    else Maybe(eBeforeContent.unparser)

  private lazy val eUnparser: Maybe[Unparser] =
    if (eValue.isEmpty) Maybe.Nope
    else Maybe(eValue.unparser)

  private lazy val eAfterUnparser: Maybe[Unparser] =
    if (eAfterValue.isEmpty) Maybe.Nope
    else Maybe(eAfterValue.unparser)

  override lazy val unparser: Unparser = {
    if (context.isOutputValueCalc) {
      new ElementOVCSpecifiedLengthUnparser(context.erd,
        context.maybeUnparseTargetLengthInBitsEv,
        uSetVars,
        eBeforeUnparser,
        eUnparser,
        eAfterUnparser)
    } else if ((context.lengthKind _eq_ LengthKind.Explicit) ||
      (context.isSimpleType &&
        (context.lengthKind _eq_ LengthKind.Implicit) &&
        (context.impliedRepresentation _eq_ Representation.Text))) {

      new ElementSpecifiedLengthUnparser(context.erd,
        context.maybeUnparseTargetLengthInBitsEv,
        uSetVars,
        eBeforeUnparser,
        eUnparser,
        eAfterUnparser)
    } else {
      subComb.unparser
    }
  }

}

case class ElementUnused(ctxt: ElementBase)
  extends Terminal(ctxt, ctxt.shouldAddFill ||
    ctxt.shouldCheckExcessLength) {
  override def parser = new NadaParser(ctxt.erd)

  override lazy val unparser: Unparser = new ElementUnusedUnparser(ctxt.erd,
    ctxt.maybeUnparseTargetLengthInBitsEv.get,
    ctxt.maybeLengthEv,
    ctxt.maybeCharsetEv,
    ctxt.maybeLiteralNilEv,
    ctxt.fillByteEv)
}

case class OnlyPadding(ctxt: ElementBase)
  extends Terminal(ctxt, ctxt.shouldAddPadding)
  with Padded {

  override def parser = new NadaParser(ctxt.erd)

  override lazy val unparser: Unparser = {
    Assert.invariant(ctxt.maybeUnparseMinOrTargetLengthInBitsEv.isDefined)
    val mmtlev = ctxt.maybeUnparseMinOrTargetLengthInBitsEv.get
    new OnlyPaddingUnparser(ctxt.erd,
      mmtlev,
      ctxt.maybeLengthEv,
      ctxt.maybeCharsetEv,
      ctxt.maybeLiteralNilEv,
      unparsingPadChar)
  }
}

case class NilLiteralCharacter(ctxt: ElementBase)
  extends Terminal(ctxt, ctxt.maybeUnparseTargetLengthInBitsEv.isDefined &&
    ctxt.isNillable && ctxt.nilKind == NilKind.LiteralCharacter)
  with Padded {

  override def parser = new NadaParser(ctxt.erd)

  private lazy val nilLitCharacter = ctxt.cookedNilValuesForUnparse.head(0)

  override lazy val unparser: Unparser =
    new NilLiteralCharacterUnparser(ctxt.erd,
      ctxt.maybeUnparseTargetLengthInBitsEv.get,
      ctxt.maybeLengthEv,
      ctxt.maybeCharsetEv,
      nilLitCharacter)
}

case class RightCenteredPadding(ctxt: ElementBase)
  extends Terminal(ctxt, ctxt.shouldAddPadding)
  with Padded {
  override def parser = new NadaParser(ctxt.erd)

  override lazy val unparser: Unparser =
    new RightCenteredPaddingUnparser(ctxt.erd,
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
    new LeftCenteredPaddingUnparser(ctxt.erd,
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

  override lazy val unparser: Unparser = new RightFillUnparser(ctxt.erd,
    ctxt.maybeUnparseTargetLengthInBitsEv.get,
    ctxt.maybeLengthEv,
    ctxt.maybeCharsetEv,
    ctxt.maybeLiteralNilEv,
    ctxt.fillByteEv,
    unparsingPadChar)
}

case class OVCRetry(ctxt: ElementBase, v: Gram)
  extends Terminal(ctxt, true) {
  override def parser = v.parser

  override def unparser = new OVCRetryUnparser(ctxt.erd,
    ctxt.maybeUnparseTargetLengthInBitsEv, v.unparser)
}

case class CaptureContentLengthStart(ctxt: ElementBase)
  extends Terminal(ctxt, true) {
  override def parser =
    if (ctxt.isReferencedByContentLengthParserExpressions)
      new CaptureStartOfContentLengthParser(ctxt.erd)
    else
      new NadaParser(ctxt.erd)

  override lazy val unparser: Unparser =
    // TODO: This content length start is added when maybeFixedLengthInBits is
    // defined because it allows us to set absolute start bit positions of the
    // DOS, even when there are things like padding and OVC that can cause
    // suspensions that result in relative bit positions. However, we really
    // only need this if there are going to be suspensions, not on all fixed
    // length elements. Otherwise, we're capturing content length for no reason
    // (unless it is referenced in a contentLength expression). We should
    // improve this check so that this unparser can be optimized out if there
    // will not be any suspensions.
    if (ctxt.isReferencedByContentLengthUnparserExpressions ||
        (ctxt.maybeFixedLengthInBits.isDefined && ctxt.couldHaveSuspensions))
      new CaptureStartOfContentLengthUnparser(ctxt.erd)
    else
      new NadaUnparser(ctxt.erd)
}

case class CaptureContentLengthEnd(ctxt: ElementBase)
  extends Terminal(ctxt, true) {
  override def parser =
    if (ctxt.isReferencedByContentLengthParserExpressions)
      new CaptureEndOfContentLengthParser(ctxt.erd)
    else
      new NadaParser(ctxt.erd)

  override lazy val unparser: Unparser =
    // TODO: This content length start is added when maybeFixedLengthInBits is
    // defined because it allows us to set absolute start bit positions of the
    // DOS, even when there are things like padding and OVC that can cause
    // suspensions that result in relative bit positions. However, we really
    // only need this if there are going to be suspensions, not on all fixed
    // length elements. Otherwise, we're capturing content length for no reason
    // (unless it is referenced in a contentLength expression). We should
    // improve this check so that this unparser can be optimized out if there
    // will not be any suspensions.
    if (ctxt.isReferencedByContentLengthUnparserExpressions ||
        (ctxt.maybeFixedLengthInBits.isDefined && ctxt.couldHaveSuspensions))
      new CaptureEndOfContentLengthUnparser(ctxt.erd, ctxt.maybeFixedLengthInBits)
    else
      new NadaUnparser(ctxt.erd)
}

case class CaptureValueLengthStart(ctxt: ElementBase)
  extends Terminal(ctxt, true) {
  override def parser =
    if (ctxt.isReferencedByValueLengthParserExpressions) {
      // For simple elements with text representation, valueLength is captured in
      // individual parsers since they handle removing delimiters and padding.
      //
      // For complex elements with specified length, valueLength is captured in
      // the specified length parsers, since they handle skipping unused
      // element regions. For complex elements, this means lengthKind is not
      // implicit or delimited.
      //
      // For all other elements, we can just use the Capture*ValueLength parsers.
      if ((ctxt.isSimpleType && ctxt.impliedRepresentation == Representation.Text) ||
        (ctxt.isComplexType && (ctxt.lengthKind != LengthKind.Implicit && ctxt.lengthKind != LengthKind.Delimited)))
        new NadaParser(ctxt.erd)
      else
        new CaptureStartOfValueLengthParser(ctxt.erd)
    } else
      new NadaParser(ctxt.erd)

  override lazy val unparser: Unparser =
    if (ctxt.isReferencedByValueLengthUnparserExpressions) {
      new CaptureStartOfValueLengthUnparser(ctxt.erd)
    } else
      new NadaUnparser(ctxt.erd)
}

case class CaptureValueLengthEnd(ctxt: ElementBase)
  extends Terminal(ctxt, true) {
  override def parser =
    if (ctxt.isReferencedByValueLengthParserExpressions) {
      // For simple elements with text representation, valueLength is captured in
      // individual parsers since they handle removing delimiters and padding.
      //
      // For complex elements with specified length, valueLength is captured in
      // the specified length parsers, since they handle skipping unused
      // element regions. For complex elements, this means lengthKind is not
      // implicit or delimited.
      //
      // For all other elements, we can just use the Capture*ValueLength parsers.
      if ((ctxt.isSimpleType && ctxt.impliedRepresentation == Representation.Text) ||
        (ctxt.isComplexType && (ctxt.lengthKind != LengthKind.Implicit && ctxt.lengthKind != LengthKind.Delimited)))
        new NadaParser(ctxt.erd)
      else
        new CaptureEndOfValueLengthParser(ctxt.erd)
    } else
      new NadaParser(ctxt.erd)

  override lazy val unparser: Unparser =
    if (ctxt.isReferencedByValueLengthUnparserExpressions) {
      new CaptureEndOfValueLengthUnparser(ctxt.erd)
    } else
      new NadaUnparser(ctxt.erd)
}

/*
 * new stuff for specified length unparsing above here.
 */

class ElementParseAndUnspecifiedLength(context: ElementBase, eBeforeGram: Gram, eGram: Gram, eAfterGram: Gram)
  extends ElementCombinatorBase(context, eBeforeGram, eGram, eAfterGram) {

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
        eAfterParser)
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
        new ElementUnspecifiedLengthUnparser(context.erd, uSetVar, eBeforeUnparser, eUnparser, eAfterUnparser)
      }
    } else {
      // dfdl:inputValueCalc case.
      // This unparser will assume the events are in the event stream, having been inferred and put
      // in place by the next element resolver.
      new ElementUnparserNoRep(context.erd, uSetVar)
    }
  }
}

class ChoiceElementCombinator(context: ElementBase, eGramBefore: Gram, eGram: Gram, eAfterGram: Gram)
  extends ElementCombinatorBase(context, eGramBefore, eGram, eAfterGram) with HasNoUnparser {

  lazy val parser: Parser = new ChoiceElementParser(
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

}

abstract class ElementCombinatorBase(context: ElementBase, eGramBefore: Gram, eGram: Gram, eGramAfter: Gram)
  extends NamedGram(context) {

  // The order of things matters in some cases, so to be consistent we'll always use the
  // same order even when it doesn't matter

  // The order of evaluation of statements is:
  // - pattern discriminators
  // - pattern asserts
  // - the parsing of the element itself
  // - setVariables
  // - test discriminators (must be attempted even if the parsing of element or setVariable statements fail)
  // - test asserts

  // requiredEvaluations(patDiscrim, patAssert, eGram, setVar, testDiscrim, testAssert)
  // Note: above not needed as these are ALWAYS evaluated below.

  lazy val patDiscrim = {
    val pd = context.discriminatorStatements.filter(_.testKind == TestKind.Pattern)
    Assert.invariant(pd.size <= 1)
    if (pd.size == 0) {
      Maybe.Nope
    } else {
      Maybe(pd(0).gram.parser)
    }
  }
  lazy val patAssert = context.assertStatements.filter(_.testKind == TestKind.Pattern).map(_.gram.parser).toArray
  lazy val pSetVar = context.setVariableStatements.map(_.gram.parser).toArray
  lazy val testDiscrim = {
    val td = context.discriminatorStatements.filter(_.testKind == TestKind.Expression)
    Assert.invariant(td.size <= 1)
    if (td.size == 0) {
      Maybe.Nope
    } else {
      Maybe(td(0).gram.parser)
    }
  }
  lazy val testAssert = context.assertStatements.filter(_.testKind == TestKind.Expression).map(_.gram.parser).toArray

  lazy val eBeforeParser: Maybe[Parser] =
    if (eGramBefore.isEmpty) Maybe.Nope
    else Maybe(eGramBefore.parser)

  lazy val eParser: Maybe[Parser] =
    if (eGram.isEmpty) Maybe.Nope
    else Maybe(eGram.parser)

  lazy val eAfterParser: Maybe[Parser] =
    if (eGramAfter.isEmpty) Maybe.Nope
    else Maybe(eGramAfter.parser)

  def parser: Parser

  lazy val uSetVar = context.setVariableStatements.map(_.gram.unparser).toArray

  lazy val eBeforeUnparser: Maybe[Unparser] =
    if (eGramBefore.isEmpty) Maybe.Nope
    else Maybe(eGramBefore.unparser)

  lazy val eUnparser: Maybe[Unparser] =
    if (eGram.isEmpty) Maybe.Nope
    else Maybe(eGram.unparser)

  lazy val eAfterUnparser: Maybe[Unparser] =
    if (eGramAfter.isEmpty) Maybe.Nope
    else Maybe(eGramAfter.unparser)

  def unparser: Unparser

}
