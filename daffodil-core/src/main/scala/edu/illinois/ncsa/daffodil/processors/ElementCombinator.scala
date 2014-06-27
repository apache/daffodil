package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.grammar.NamedGram
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TestKind
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.processors.xpath.DFDLCheckConstraintsFunction
import edu.illinois.ncsa.daffodil.api.ValidationMode
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._

object ElementCombinator {
  def apply(context: ElementBase, eGram: Gram, eAfterGram: Gram) = {
    if (context.isRepresented)
      new ElementCombinator(context, eGram, eAfterGram)
    else
      new ElementCombinatorNoRep(context, eGram, eAfterGram)
  }
}

object ChoiceElementCombinator {
  def apply(context: ElementBase, eGram: Gram, eAfterGram: Gram) = {
    new ChoiceElementCombinator(context, eGram, eAfterGram)
  }
}

class ElementCombinator(context: ElementBase, eGram: Gram, eAfterGram: Gram)
  extends ElementCombinatorBase(context, eGram, eAfterGram: Gram) {
  def move(start: PState) {
    start.mpstate.moveOverOneGroupIndexOnly
    start.mpstate.moveOverOneElementChildOnly
  }
  def parseElementBegin(pstate: PState): PState = {
    val currentElement = Infoset.newElement(context, isHidden)

    log(LogLevel.Debug, "currentElement = %s", currentElement)
    val priorElement = pstate.infoset
    priorElement.addElement(currentElement)
    log(LogLevel.Debug, "priorElement = %s", priorElement)
    val postState = pstate.withParent(currentElement)
    postState
  }

  def parseElementEnd(pstate: PState): PState = {
    val currentElement = pstate.parentElement

    val shouldValidate = pstate.mpstate.dataProc.getValidationMode != ValidationMode.Off
    val postValidate =
      if (shouldValidate && context.isSimpleType) {
        // Execute checkConstraints
        val resultState = validate(pstate)
        resultState
      } else pstate

    // Assert.invariant(currentElement.getName() != "_document_" )
    val priorElement = currentElement.parent
    log(LogLevel.Debug, "priorElement = %s", priorElement)
    val postState = postValidate.withParent(priorElement)
    move(pstate)
    postState
  }
}

class ElementCombinatorNoRep(context: ElementBase, eGram: Gram, eAfterGram: Gram)
  extends ElementCombinator(context, eGram, eAfterGram) {
  // if there is no rep (inputValueCalc), then we do create a new child so that index must advance,
  // but we don't create anything new as far as the group is concerned, and we don't want 
  // the group 'thinking' that there's a prior sibling inside the group and placing a 
  // separator after it. So in the case of NoRep, we don't advance group child, just element child.
  override def move(state: PState) {
    state.mpstate.moveOverOneElementChildOnly
  }
}

class ChoiceElementCombinator private (context: ElementBase, eGram: Gram, eAfterGram: Gram)
  extends ElementCombinatorBase(context, eGram, eAfterGram: Gram) {
  def move(state: PState) = {}

  /**
   * ElementBegin just adds the element we are constructing to the infoset and changes
   * the state to be referring to this new element as what we're parsing data into.
   */
  def parseElementBegin(pstate: PState): PState = {
    val currentElement = Infoset.newElement(context, isHidden)

    log(LogLevel.Debug, "currentElement = %s", currentElement)
    pstate
  }

  // We don't want to modify the state here except
  // for validation.
  def parseElementEnd(pstate: PState): PState = {
    val currentElement = pstate.parentElement

    val shouldValidate = pstate.mpstate.dataProc.getValidationMode != ValidationMode.Off
    val postValidate =
      if (shouldValidate && context.isSimpleType) {
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

  val patDiscrim = context.discriminatorStatements.filter(_.testKind == TestKind.Pattern).map(_.gram)
  val patAssert = context.assertStatements.filter(_.testKind == TestKind.Pattern).map(_.gram)
  val setVar = context.setVariableStatements.map(_.gram)
  val testDiscrim = context.discriminatorStatements.filter(_.testKind == TestKind.Expression).map(_.gram)
  val testAssert = context.assertStatements.filter(_.testKind == TestKind.Expression).map(_.gram)

  val eParser = eGram.parser
  val eAfterParser = eGramAfter.parser

  val isHidden = context.isHidden

  def parser: Parser = new StatementElementParser(context, new ElementBeginParser(context), new ElementEndParser(context))

  def unparser: Unparser = new Unparser(context) {
    def unparse(start: UState): UState = {
      // FIXME: setVariables have to execute. We don't do asserts and discriminators when unparsing however.
      val eUnParser = eGram.unparser
      val postEState = eUnParser.unparse(start)
      postEState
    }
  }

  def move(pstate: PState): Unit // implement for different kinds of "moving over to next thing"
  def parseElementBegin(pstate: PState): PState
  def parseElementEnd(pstate: PState): PState

  def validate(pstate: PState): PState = {
    val currentElement = pstate.parentElement
    
    if (currentElement.wasCheckConstraintsRun) { return pstate }

    val resultState = DFDLCheckConstraintsFunction.validate(pstate) match {
      case Right(boolVal) => {
        log(LogLevel.Debug, "Validation succeeded for %s", currentElement.toBriefXML)
        pstate // Success, do not mutate state.
      }
      case Left(failureMessage) => {
        log(LogLevel.Debug,
          "Validation failed for %s due to %s. The element value was %s.",
          context.toString, failureMessage, currentElement.toBriefXML)
        pstate.withValidationError("%s failed dfdl:checkConstraints due to %s",
          context.toString, failureMessage)
      }
    }

    resultState
  }

  class ElementBeginParser(context: ElementBase) extends PrimParser(this, context) {
    def parse(pstate: PState): PState = parseElementBegin(pstate)
    override def toBriefXML(depthLimit: Int = -1): String = {
      "<ElementBegin name='" + context.name + "'/>"
    }
  }

  class ElementEndParser(context: ElementBase) extends PrimParser(this, context) {
    def parse(pstate: PState): PState = parseElementEnd(pstate)
    override def toBriefXML(depthLimit: Int = -1): String = {
      "<ElementEnd name='" + context.name + "'/>"
    }
  }

  class StatementElementParser(context: ElementBase, beginParser: Parser, endParser: Parser) extends PrimParser(this, context) {

    Assert.invariant(testDiscrim.size <= 1)
    Assert.invariant(patDiscrim.size <= 1)

    override def toBriefXML(depthLimit: Int = -1): String = {
      if (depthLimit == 0) "..." else
        "<Element name='" + context.name + "'>" +
          patDiscrim.map { _.parser.toBriefXML(depthLimit - 1) }.mkString +
          patAssert.map { _.parser.toBriefXML(depthLimit - 1) }.mkString +
          eParser.toBriefXML(depthLimit - 1) +
          setVar.map { _.parser.toBriefXML(depthLimit - 1) }.mkString +
          testDiscrim.map { _.parser.toBriefXML(depthLimit - 1) }.mkString +
          testAssert.map { _.parser.toBriefXML(depthLimit - 1) }.mkString +
          eAfterParser.toBriefXML(depthLimit - 1) +
          "</Element name='" + context.name + "'>"
    }

    val patDiscrimParser = patDiscrim.map(_.parser)
    val patAssertParser = patAssert.map(_.parser)
    val setVarParser = setVar.map(_.parser)
    val testDiscrimParser = testDiscrim.map(_.parser)
    val testAssertParser = testAssert.map(_.parser)

    def parse(pstate: PState): PState = {
      //Removed checks now done at compilation

      var afterPatDisc = pstate //we're first so don't do .withPos(pstate.bitPos, pstate.charPos)
      patDiscrimParser.foreach(d => {
        afterPatDisc = d.parse1(afterPatDisc, context)
        // Pattern fails at the start of the Element
        if (afterPatDisc.status != Success) { return afterPatDisc }
      })

      // now here we backup and run the pattern Asserts 
      // against the data at the start of the element's representation again.
      var afterPatAssrt = afterPatDisc.withPos(pstate.bitPos, pstate.charPos, pstate.inStream.reader)
      patAssertParser.foreach(d => {
        afterPatAssrt = d.parse1(afterPatAssrt, context)
        // Pattern fails at the start of the Element
        if (afterPatAssrt.status != Success) { return afterPatAssrt }
      })

      // backup again. If all pattern discriminators and/or asserts
      // have passed, now we parse the element. But we backup
      // as if the pattern matching had not advanced the state.
      val beforeEState = afterPatAssrt.withPos(pstate.bitPos, pstate.charPos, pstate.inStream.reader)

      val postElementStartState =
        if (beginParser.isInstanceOf[EmptyGramParser]) beforeEState
        else beginParser.parse1(beforeEState, context)

      if (postElementStartState.status != Success) return postElementStartState

      val postEState =
        if (eParser.isInstanceOf[EmptyGramParser]) postElementStartState
        else eParser.parse1(postElementStartState, context)

      var someSetVarFailed: Maybe[PState] = Nope

      var afterSetVar = postEState
      if (postEState.status == Success) {
        setVarParser.foreach(d => {
          val afterOneSetVar = d.parse1(afterSetVar, context)
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
        afterTestDisc = d.parse1(afterTestDisc, context)
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
        afterTestAssrt = d.parse1(afterTestAssrt, context)
        // Tests fail at the end of the Element
        if (afterTestAssrt.status != Success) { return afterTestAssrt }
      })

      val postAfterState =
        if (eAfterParser.isInstanceOf[EmptyGramParser]) afterTestAssrt
        else eAfterParser.parse1(afterTestAssrt, context)

      if (postAfterState.status != Success) return postAfterState

      val postElementEndState = endParser.parse1(postAfterState, context)

      postElementEndState //afterTestAssrt
    }
  }
}
