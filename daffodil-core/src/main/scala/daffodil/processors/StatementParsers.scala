package daffodil.processors
import daffodil.dsom.DFDLAssert
import daffodil.dsom.DFDLDiscriminator
import daffodil.dsom.DFDLSetVariable
import daffodil.dsom.ElementBase
import daffodil.grammar.Gram
import daffodil.grammar.NamedGram
import daffodil.schema.annotation.props.gen.TestKind
import daffodil.exceptions.Assert

object StmtEval{
  def apply(context: ElementBase, eGram: => Gram) = new StmtEval(context, eGram)
}

class StmtEval(context: ElementBase, eGram: => Gram)
  extends NamedGram(context) {
  lazy val diagnosticChildren = List(eGram) ++ patAssert ++ testAssert ++ patDiscrim ++ testDiscrim ++ setVar
  val patAssert = context.statements.filter(x => x match {
    case a: DFDLAssert => a.testKind == TestKind.Pattern
    case _ => false
  }).map(_.gram)
  val testAssert = context.statements.filter(x => x match {
    case a: DFDLAssert => a.testKind == TestKind.Expression
    case _ => false
  }).map(_.gram)
  val patDiscrim = context.statements.filter(x => x match {
    case a: DFDLDiscriminator => a.testKind == TestKind.Pattern
    case _ => false
  }).map(_.gram)
  val testDiscrim = context.statements.filter(x => x match {
    case a: DFDLDiscriminator => a.testKind == TestKind.Expression
    case _ => false
  }).map(_.gram)
  val setVar = context.statements.filter(x => x match {
    case a: DFDLSetVariable => true
    case _ => false
  }).map(_.gram)

  val eParser = eGram.parser

  def parser: Parser = new Parser(context) {
    def toBriefXML(depthLimit: Int = -1): String = {
      if (depthLimit == 0) "..." else
        "<StmtEval>" + eParser.toBriefXML(depthLimit - 1) +
         setVar.mkString + testDiscrim.mkString + 
         patDiscrim.mkString + testAssert.mkString + 
         patAssert.mkString +
          "</StmtEval>"
    }

    def parse(pstate: PState): PState = {
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
  
  def unparser: Unparser = new Unparser(context){
    def unparse(start: UState): UState = {
      // We want to delegate to the grammar to handle this
      // as the dfdl statements have nothing to do.
      val eUnParser = eGram.unparser
      val postEState = eUnParser.unparse(start)
      postEState
    }
  }
  
}
