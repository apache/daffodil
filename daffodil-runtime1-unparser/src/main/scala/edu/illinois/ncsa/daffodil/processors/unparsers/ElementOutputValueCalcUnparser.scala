package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.processors.DINode
import edu.illinois.ncsa.daffodil.processors.DISimple
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.util.LogLevel
import java.nio.charset.MalformedInputException
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset

class ElementOutputValueCalcUnparser(erd: ElementRuntimeData)
  extends Unparser(erd) with TextUnparserRuntimeMixin {

  Assert.invariant(erd.outputValueCalcExpr.isDefined)
  val expr = erd.outputValueCalcExpr.get

  override lazy val childProcessors = Nil

  def unparse(ustate: UState): Unit = {

    // FIXME: This is broken. Can't work ultimately because expressions require variables which 
    // require model-groups. Just the infoset events... lacks the model-group traversal.
    // puller.pullUntilExpressionCanBeEvaluated(ustate)
    // We can pull to force nodes to be added to the infoset for OVC and defaultables, 
    // but we cannot pull events in a non-schema-specific manner and actually evaluate 
    // expressions. 

    //
    // Once we get here, then we're ready to evaluate the expression
    // 
    val value = expr.evaluate(ustate)
    ustate.currentInfosetNode.get.asInstanceOf[DISimple].setDataValue(value)

    // Now that we have the value, some other unparser will end up 
    // unparsing this value. Not here.
  }

}