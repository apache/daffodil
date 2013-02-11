package daffodil.processors
import daffodil.dsom.DFDLAssert
import daffodil.dsom.DFDLDiscriminator
import daffodil.dsom.DFDLSetVariable
import daffodil.dsom.ElementBase
import daffodil.grammar.Gram
import daffodil.grammar.NamedGram
import daffodil.schema.annotation.props.gen.TestKind
import daffodil.exceptions.Assert

object StmtEval {
  def apply(context: ElementBase, eGram: => Gram) = {
    // little optimization here. If there are no statements (most common case), then let's 
    // shortcut and just use the guts parser.
    val hasStatements = context.notNewVariableInstanceStatements.size > 0
    if (hasStatements)
      new StmtEval(context, eGram)
    else
      eGram
  }
}

class StmtEval(context: ElementBase, eGram: Gram)
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

  lazy val diagnosticChildren: DiagnosticsList = patDiscrim ++ patAssert ++ List(eGram) ++ setVar ++ testDiscrim ++ testAssert

  val patDiscrim = context.discriminatorStatements.filter(_.testKind == TestKind.Pattern).map(_.gram)
  val patAssert = context.assertStatements.filter(_.testKind == TestKind.Pattern).map(_.gram)
  val setVar = context.setVariableStatements.map(_.gram)
  val testDiscrim = context.discriminatorStatements.filter(_.testKind == TestKind.Expression).map(_.gram)
  val testAssert = context.assertStatements.filter(_.testKind == TestKind.Expression).map(_.gram)

  val eParser = eGram.parser

  def parser: Parser = new StatementElementParser(context)

  class StatementElementParser(context: ElementBase) extends PrimParser(this, context) {

    Assert.invariant(context.notNewVariableInstanceStatements.size > 0)
    Assert.invariant(testDiscrim.size <= 1)
    Assert.invariant(patDiscrim.size <= 1)

    override def toBriefXML(depthLimit: Int = -1): String = {
      if (depthLimit == 0) "..." else
        "<StmtEval>" +
          patDiscrim.map { _.parser.toBriefXML(depthLimit - 1) }.mkString +
          patAssert.map { _.parser.toBriefXML(depthLimit - 1) }.mkString +
          eParser.toBriefXML(depthLimit - 1) +
          setVar.map { _.parser.toBriefXML(depthLimit - 1) }.mkString +
          testDiscrim.map { _.parser.toBriefXML(depthLimit - 1) }.mkString +
          testAssert.map { _.parser.toBriefXML(depthLimit - 1) }.mkString +
          "</StmtEval>"
    }

    def parse(pstate: PState): PState = {
      //Removed checks now done at compilation

      var afterPatDisc = pstate.withPos(pstate.bitPos, pstate.charPos)
      patDiscrim.map(_.parser).foreach(d => {
        afterPatDisc = d.parse1(afterPatDisc, context)
        // Pattern fails at the start of the Element
        if (afterPatDisc.status != Success) { return afterPatDisc }
      })

      var afterPatAssrt = afterPatDisc.withPos(pstate.bitPos, pstate.charPos)
      patAssert.map(_.parser).foreach(d => {
        afterPatAssrt = d.parse1(afterPatAssrt, context)
        // Pattern fails at the start of the Element
        if (afterPatAssrt.status != Success) { return afterPatAssrt }
      })

      val postEState = eParser.parse1(afterPatAssrt, context)

      var someSetVarFailed: Option[PState] = None

      var afterSetVar = postEState
      if (postEState.status == Success) {
        setVar.map(_.parser).foreach(d => {
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
            someSetVarFailed = Some(afterOneSetVar)
            afterSetVar = afterOneSetVar
          }
        })
      }

      var afterTestDisc = afterSetVar
      testDiscrim.map(_.parser).foreach(d => {
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
      testAssert.map(_.parser).foreach(d => {
        afterTestAssrt = d.parse1(afterTestAssrt, context)
        // Tests fail at the end of the Element
        if (afterTestAssrt.status != Success) { return afterTestAssrt }
      })

      afterTestAssrt
    }
  }

  def unparser: Unparser = new Unparser(context) {
    def unparse(start: UState): UState = {
      // FIXME: setVariables have to execute. We don't do asserts and discriminators when unparsing however.
      val eUnParser = eGram.unparser
      val postEState = eUnParser.unparse(start)
      postEState
    }
  }

}
