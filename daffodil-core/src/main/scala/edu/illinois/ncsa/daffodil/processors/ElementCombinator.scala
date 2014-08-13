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

abstract class StatementElementParserBase(
  rd: RuntimeData,
  name: String,
  patDiscrimParser: Seq[Parser],
  patAssertParser: Seq[Parser],
  setVarParser: Seq[Parser],
  testDiscrimParser: Seq[Parser],
  testAssertParser: Seq[Parser],
  eParser: Option[Parser],
  eAfterParser: Option[Parser])
  extends PrimParser(rd) {

  Assert.invariant(testDiscrimParser.size <= 1)
  Assert.invariant(patDiscrimParser.size <= 1)

  def move(pstate: PState): Unit // implement for different kinds of "moving over to next thing"
  def parseBegin(pstate: PState): PState
  def parseEnd(pstate: PState): PState

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<Element name='" + name + "'>" +
        patDiscrimParser.map { _.toBriefXML(depthLimit - 1) }.mkString +
        patAssertParser.map { _.toBriefXML(depthLimit - 1) }.mkString +
        eParser.map { _.toBriefXML(depthLimit - 1) }.getOrElse("") +
        setVarParser.map { _.toBriefXML(depthLimit - 1) }.mkString +
        testDiscrimParser.map { _.toBriefXML(depthLimit - 1) }.mkString +
        testAssertParser.map { _.toBriefXML(depthLimit - 1) }.mkString +
        eAfterParser.map { _.toBriefXML(depthLimit - 1) }.getOrElse("") +
        "</Element name='" + name + "'>"
  }

  def validate(pstate: PState): PState = {
    val currentElement = pstate.thisElement

    if (currentElement.valid.isDefined) { return pstate }

    val resultState = DFDLCheckConstraintsFunction.validate(pstate) match {
      case Right(boolVal) => {
        log(LogLevel.Debug, "Validation succeeded for %s", currentElement.toXML)
        currentElement.setValid(true)
        pstate // Success, do not mutate state.
      }
      case Left(failureMessage) => {
        log(LogLevel.Debug,
          "Validation failed for %s due to %s. The element value was %s.",
          context.toString, failureMessage, currentElement.toXML)
        pstate.withValidationError("%s failed dfdl:checkConstraints due to %s",
          context.toString, failureMessage)
        currentElement.setValid(false)
        pstate
      }
    }
    resultState
  }

  def parse(pstate: PState): PState = {
    //Removed checks now done at compilation

    var afterPatDisc = pstate //we're first so don't do .withPos(pstate.bitPos, pstate.charPos)
    patDiscrimParser.foreach(d => {
      afterPatDisc = d.parse1(afterPatDisc, rd)
      // Pattern fails at the start of the Element
      if (afterPatDisc.status != Success) { return afterPatDisc }
    })

    // now here we backup and run the pattern Asserts 
    // against the data at the start of the element's representation again.
    var afterPatAssrt = afterPatDisc.withPos(pstate.bitPos, pstate.charPos, pstate.inStream.reader)
    patAssertParser.foreach(d => {
      afterPatAssrt = d.parse1(afterPatAssrt, rd)
      // Pattern fails at the start of the Element
      if (afterPatAssrt.status != Success) { return afterPatAssrt }
    })

    // backup again. If all pattern discriminators and/or asserts
    // have passed, now we parse the element. But we backup
    // as if the pattern matching had not advanced the state.
    val beforeEState = afterPatAssrt.withPos(pstate.bitPos, pstate.charPos, pstate.inStream.reader)

    val postElementStartState = parseBegin(beforeEState)

    if (postElementStartState.status != Success) return postElementStartState

    val postEState = eParser.map { eParser =>
      eParser.parse1(postElementStartState, rd)
    }.getOrElse(postElementStartState)

    var someSetVarFailed: Maybe[PState] = Nope

    var afterSetVar = postEState
    if (postEState.status == Success) {
      setVarParser.foreach(d => {
        val afterOneSetVar = d.parse1(afterSetVar, rd)
        if (afterOneSetVar.status == Success) {
          afterSetVar = afterOneSetVar
        } else {
          // a setVariable statement failed. But we want to continue to try 
          // more of the setVariable statements, as they may be necessary
          // to evaluate the test discriminator below, and some might 
          // be successful even if one fails, allowing the discriminator to be true.
          //
          // So it's a bit odd, but we're going to just keep parsing using this
          // failed state as the input to the next setVariable parse step.
          someSetVarFailed = One(afterOneSetVar)
          afterSetVar = afterOneSetVar
        }
      })
    }

    var afterTestDisc = afterSetVar
    testDiscrimParser.foreach(d => {
      afterTestDisc = d.parse1(afterTestDisc, rd)
      // Tests fail at the end of the Element
      if (afterTestDisc.status != Success) { return afterTestDisc }
    })

    // 
    // We're done with the discriminator, so now we revisit the set variable statements.
    // If a failure occurred there, then now we can fail out right here.
    // 
    someSetVarFailed.exists { return _ }

    // Element evaluation failed, return
    if (postEState.status != Success) { return postEState }

    var afterTestAssrt = afterTestDisc
    testAssertParser.foreach(d => {
      afterTestAssrt = d.parse1(afterTestAssrt, rd)
      // Tests fail at the end of the Element
      if (afterTestAssrt.status != Success) { return afterTestAssrt }
    })

    val postAfterState = eAfterParser.map { eAfterParser =>
      eAfterParser.parse1(afterTestAssrt, rd)
    }.getOrElse(afterTestAssrt)

    if (postAfterState.status != Success) return postAfterState

    val postElementEndState = parseEnd(postAfterState)

    postElementEndState //afterTestAssrt
  }
}

class StatementElementParser(
  erd: ElementRuntimeData,
  name: String,
  patDiscrim: Seq[Parser],
  patAssert: Seq[Parser],
  setVar: Seq[Parser],
  testDiscrim: Seq[Parser],
  testAssert: Seq[Parser],
  eParser: Option[Parser],
  eAfterParser: Option[Parser])
  extends StatementElementParserBase(
    erd,
    name,
    patDiscrim,
    patAssert,
    setVar,
    testDiscrim,
    testAssert,
    eParser,
    eAfterParser) {

  def move(start: PState) {
    start.mpstate.moveOverOneGroupIndexOnly
    start.mpstate.moveOverOneElementChildOnly
  }

  def parseBegin(pstate: PState): PState = {
    val currentElement = Infoset.newElement(erd)

    log(LogLevel.Debug, "currentElement = %s", currentElement)
    val priorElement = pstate.infoset
    priorElement match {
      case ct: DIComplex => ct.addChild(currentElement)
      case st: DISimple => {
        // don't add as a child. This corner case
        // is just about tests where the root node is 
        // a simple element. 
      }
    }
    log(LogLevel.Debug, "priorElement = %s", priorElement)
    val postState = pstate.withParent(currentElement)
    postState
  }

  def parseEnd(pstate: PState): PState = {
    val currentElement = pstate.thisElement

    val shouldValidate = pstate.mpstate.dataProc.getValidationMode != ValidationMode.Off
    val postValidate =
      if (shouldValidate && erd.isSimpleType) {
        // Execute checkConstraints
        val resultState = validate(pstate)
        resultState
      } else pstate

    // Assert.invariant(currentElement.getName() != "_document_" )
    val priorElement = currentElement.parent
    log(LogLevel.Debug, "priorElement = %s", priorElement)
    val postState =
      if (priorElement.isDefined) postValidate.withParent(priorElement.get)
      else postValidate
    move(pstate)
    postState
  }
}

class StatementElementParserNoRep(
  erd: ElementRuntimeData,
  name: String,
  patDiscrim: Seq[Parser],
  patAssert: Seq[Parser],
  setVar: Seq[Parser],
  testDiscrim: Seq[Parser],
  testAssert: Seq[Parser],
  eParser: Option[Parser],
  eAfterParser: Option[Parser])
  extends StatementElementParser(
    erd,
    name,
    patDiscrim,
    patAssert,
    setVar,
    testDiscrim,
    testAssert,
    eParser,
    eAfterParser) {

  // if there is no rep (inputValueCalc), then we do create a new child so that index must advance,
  // but we don't create anything new as far as the group is concerned, and we don't want 
  // the group 'thinking' that there's a prior sibling inside the group and placing a 
  // separator after it. So in the case of NoRep, we don't advance group child, just element child.
  override def move(state: PState) {
    state.mpstate.moveOverOneElementChildOnly
  }
}

//class StatementElementArrayParser(
//  erd: ElementRuntimeData,
//  name: String,
//  patDiscrim: Seq[Parser],
//  patAssert: Seq[Parser],
//  setVar: Seq[Parser],
//  testDiscrim: Seq[Parser],
//  testAssert: Seq[Parser],
//  eParser: Option[Parser],
//  eAfterParser: Option[Parser])
//  extends StatementElementParserBase(
//    erd,
//    name,
//    patDiscrim,
//    patAssert,
//    setVar,
//    testDiscrim,
//    testAssert,
//    eParser,
//    eAfterParser) {
//  Assert.usage(erd.isArray)
//
//  def move(start: PState) {
//    start.mpstate.moveOverOneGroupIndexOnly
//    start.mpstate.moveOverOneElementChildOnly
//  }
//
//  def parseBegin(pstate: PState): PState = {
//    Assert.invariant(erd.isArray)
//    val priorElement = pstate.complexElement
//    val maybeChild = priorElement.getChildMaybe(erd)
//    val currentElement =
//      if (maybeChild.isDefined) {
//        maybeChild.get
//      } else {
//        // no array exists into which to store the values
//        val arr = Infoset.newElement(erd)
//        priorElement.addChild(arr)
//        arr
//      }
//    log(LogLevel.Debug, "currentElement = %s", currentElement)
//    log(LogLevel.Debug, "priorElement = %s", priorElement)
//    val postState = pstate.withParent(currentElement)
//    postState
//  }
//
//  def parseEnd(pstate: PState): PState = {
//    val currentElement = pstate.thisElement
//
//    val shouldValidate = pstate.mpstate.dataProc.getValidationMode != ValidationMode.Off
//    val postValidate =
//      if (shouldValidate && erd.isSimpleType) {
//        // Execute checkConstraints
//        val resultState = validate(pstate)
//        resultState
//      } else pstate
//
//    val priorElement = currentElement.parent
//    log(LogLevel.Debug, "priorElement = %s", priorElement)
//    val postState =
//      if (priorElement.isDefined) postValidate.withParent(priorElement.get)
//      else postValidate
//    move(pstate)
//    postState
//  }
//}
//
//class StatementElementArrayParserNoRep(
//  erd: ElementRuntimeData,
//  name: String,
//  patDiscrim: Seq[Parser],
//  patAssert: Seq[Parser],
//  setVar: Seq[Parser],
//  testDiscrim: Seq[Parser],
//  testAssert: Seq[Parser],
//  eParser: Option[Parser],
//  eAfterParser: Option[Parser])
//  extends StatementElementArrayParser(
//    erd,
//    name,
//    patDiscrim,
//    patAssert,
//    setVar,
//    testDiscrim,
//    testAssert,
//    eParser,
//    eAfterParser) {
//
//  // if there is no rep (inputValueCalc), then we do create a new child so that index must advance,
//  // but we don't create anything new as far as the group is concerned, and we don't want 
//  // the group 'thinking' that there's a prior sibling inside the group and placing a 
//  // separator after it. So in the case of NoRep, we don't advance group child, just element child.
//  override def move(state: PState) {
//    state.mpstate.moveOverOneElementChildOnly
//  }
//}

class ChoiceStatementElementParser(
  erd: ElementRuntimeData,
  name: String,
  patDiscrim: Seq[Parser],
  patAssert: Seq[Parser],
  setVar: Seq[Parser],
  testDiscrim: Seq[Parser],
  testAssert: Seq[Parser],
  eParser: Option[Parser],
  eAfterParser: Option[Parser])
  extends StatementElementParserBase(
    erd,
    name,
    patDiscrim,
    patAssert,
    setVar,
    testDiscrim,
    testAssert,
    eParser,
    eAfterParser) {

  def move(state: PState) = {}

  /**
   * ElementBegin just adds the element we are constructing to the infoset and changes
   * the state to be referring to this new element as what we're parsing data into.
   */
  def parseBegin(pstate: PState): PState = {
    val currentElement = Infoset.newElement(erd)

    log(LogLevel.Debug, "currentElement = %s", currentElement)
    pstate
  }

  // We don't want to modify the state here except
  // for validation.
  def parseEnd(pstate: PState): PState = {
    val currentElement = pstate.thisElement

    val shouldValidate = pstate.mpstate.dataProc.getValidationMode != ValidationMode.Off
    val postValidate =
      if (shouldValidate && erd.isSimpleType) {
        // Execute checkConstraints
        val resultState = validate(pstate)
        resultState
      } else pstate

    val priorElement = currentElement.parent
    log(LogLevel.Debug, "priorElement = %s", priorElement)
    val postState = postValidate
    move(pstate)
    postState
  }
}
