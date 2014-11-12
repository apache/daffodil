package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.grammar.NamedGram
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TestKind
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.dpath.DFDLCheckConstraintsFunction
import edu.illinois.ncsa.daffodil.api.ValidationMode
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.dpath.DFDLCheckConstraintsFunction

class ElementCombinator(context: ElementBase, eGram: Gram, eAfterGram: Gram)
  extends ElementCombinatorBase(context, eGram, eAfterGram: Gram) {

  def parser: Parser =
    if (context.isRepresented)
      new StatementElementParser(
        context.erd,
        context.name,
        patDiscrim,
        patAssert,
        setVar,
        testDiscrim,
        testAssert,
        eParser,
        eAfterParser)
    else
      new StatementElementParserNoRep(
        context.erd,
        context.name,
        patDiscrim,
        patAssert,
        setVar,
        testDiscrim,
        testAssert,
        eParser,
        eAfterParser)
}

class ChoiceElementCombinator(context: ElementBase, eGram: Gram, eAfterGram: Gram)
  extends ElementCombinatorBase(context, eGram, eAfterGram: Gram) {

  def parser: Parser = new ChoiceStatementElementParser(
    context.erd,
    context.name,
    patDiscrim,
    patAssert,
    setVar,
    testDiscrim,
    testAssert,
    eParser,
    eAfterParser)
}

abstract class ElementCombinatorBase(context: ElementBase, eGram: Gram, eGramAfter: Gram)
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

  lazy val patDiscrim = context.discriminatorStatements.filter(_.testKind == TestKind.Pattern).map(_.gram.parser)
  lazy val patAssert = context.assertStatements.filter(_.testKind == TestKind.Pattern).map(_.gram.parser)
  lazy val setVar = context.setVariableStatements.map(_.gram.parser)
  lazy val testDiscrim = context.discriminatorStatements.filter(_.testKind == TestKind.Expression).map(_.gram.parser)
  lazy val testAssert = context.assertStatements.filter(_.testKind == TestKind.Expression).map(_.gram.parser)

  lazy val eParser: Option[Parser] = if (eGram.isEmpty) None
  else Some(eGram.parser)

  lazy val eAfterParser: Option[Parser] =
    if (eGramAfter.isEmpty) None
    else Some(eGramAfter.parser)

  def parser: Parser

}

