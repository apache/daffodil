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

class StmtEval(context: ElementBase, eGram: => Gram)
  extends NamedGram(context) {
  lazy val diagnosticChildren = List(eGram) ++ patAssert ++ testAssert ++ patDiscrim ++ testDiscrim ++ setVar

  val patAssert = context.assertStatements.filter(_.testKind == TestKind.Pattern).map(_.gram)
  val testAssert = context.assertStatements.filter(_.testKind == TestKind.Expression).map(_.gram)
  val patDiscrim = context.discriminatorStatements.filter(_.testKind == TestKind.Pattern).map(_.gram)
  val testDiscrim = context.discriminatorStatements.filter(_.testKind == TestKind.Expression).map(_.gram)
  val setVar = context.setVariableStatements.map(_.gram)

  val eParser = eGram.parser

  def parser: Parser = new StatementElementParser(context)

  class StatementElementParser(context: ElementBase) extends PrimParser(this, context) {

    Assert.invariant(context.notNewVariableInstanceStatements.size > 0)

    override def toBriefXML(depthLimit: Int = -1): String = {
      if (depthLimit == 0) "..." else
        "<StmtEval>" + eParser.toBriefXML(depthLimit - 1) +
          setVar.mkString + testDiscrim.mkString +
          patDiscrim.mkString + testAssert.mkString +
          patAssert.mkString +
          "</StmtEval>"
    }

    def parse(pstate: PState): PState = {
      //Removed checks now done at compilation

      val postEState = eParser.parse1(pstate, context)

      var afterSetVar = postEState
      if (postEState.status == Success) {
        setVar.map(_.parser).foreach(d => {
          afterSetVar = d.parse1(afterSetVar, context)
          if (afterSetVar.status != Success) { return afterSetVar }
        })
      }

      var afterTestDisc = afterSetVar
      testDiscrim.map(_.parser).foreach(d => {
        afterTestDisc = d.parse1(afterTestDisc, context)
        // Tests fail at the end of the Element
        if (afterTestDisc.status != Success) { return afterTestDisc }
      })

      var afterPatDisc = afterTestDisc.withPos(pstate.bitPos, pstate.charPos)
      patDiscrim.map(_.parser).foreach(d => {
        afterPatDisc = d.parse1(afterPatDisc, context)
        // Pattern fails at the start of the Element
        if (afterPatDisc.status != Success) { return afterPatDisc }
      })

      // Element evaluation failed, return
      if (postEState.status != Success) { return postEState }

      var afterTestAssrt = afterTestDisc
      testAssert.map(_.parser).foreach(d => {
        afterTestAssrt = d.parse1(afterTestAssrt, context)
        // Tests fail at the end of the Element
        if (afterTestAssrt.status != Success) { return afterTestAssrt }
      })

      var afterPatAssrt = afterTestAssrt.withPos(pstate.bitPos, pstate.charPos)
      patAssert.map(_.parser).foreach(d => {
        afterPatAssrt = d.parse1(afterPatAssrt, context)
        // Pattern fails at the start of the Element
        if (afterPatAssrt.status != Success) { return afterPatAssrt }
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
