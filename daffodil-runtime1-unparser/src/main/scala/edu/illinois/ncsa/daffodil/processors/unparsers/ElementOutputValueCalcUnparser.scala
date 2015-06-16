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

class ElementOutputValueCalcUnparser(erd: ElementRuntimeData, expr: CompiledExpression)
  extends Unparser(erd) with TextUnparserRuntimeMixin {

  override lazy val childProcessors = Nil
  /**
   * For outputValueCalc, we defer evaluating them, and change into a mode where
   * we just pull in the infoset until we have enough infoset to evaluate the
   * expression.
   */
  def unparse(ustate: UState): Unit = {
    Assert.invariant(ustate.mode == UnparseMode)
    setupEncoding(ustate, erd)

    ustate.mode = AccumulateNodesMode
    val currentSimple = ustate.currentInfosetNode.get.asSimple
    ustate.addDeferredElement(currentSimple)

    pullInfosetEventsUntilAbleToProceed(ustate)
    ustate.mode = UnparseMode

    try {
      //
      // If we get here, then we're ready to evaluate the expression
      // 
      val value = expr.evaluate(ustate).toString
      val outStream = ustate.dataOutputStream
      val nCharsWritten = outStream.putString(value)
      if (nCharsWritten != value.length) UE(ustate, "%s - Too many bits in field: IndexOutOfBounds. Insufficient space to write %s characters.", nom, value.length)

      log(LogLevel.Debug, "Ended at bit position " + outStream.bitPos1b)
    } catch {
      // Characters in infoset element cannot be encoded without error.
      //
      // This won't actually be thrown until encodingErrorPolicy='error' is
      // implemented. 
      //
      case m: MalformedInputException => { UnparseError(One(erd.schemaFileLocation), One(ustate.currentLocation), "%s - MalformedInputException: \n%s", nom, m.getMessage()) }
      //
      // Thrown if the length is explicit but are too many bytes/bits to
      // fit within the length.
      //
      case e: IndexOutOfBoundsException => {
        /// Per Review DFDL-475 Comments:
        // TODO: If the OFC is for a string, then there is a property called truncateSpecifiedLengthString,
        // which tells you whether you should just truncate the string in this case.
        // 
        // It needs the textStringJustification property knowledge of whether to truncate on left or right.
        UnparseError(One(erd.schemaFileLocation), One(ustate.currentLocation), "%s - Too many bits in field: IndexOutOfBounds: \n%s", nom, e.getMessage())
      }
    }

  }

  /**
   * Consumes infoset nodes - must deal with defaulting of
   * required defaultable elements.
   */
  def pullInfosetEventsUntilAbleToProceed(ustate: UState): Unit = {
    // Previously calling next would consume everything including the END
    // event which one of the enclosing unparsers needed.
    //
    // Just calling 'peek' should be enough as the 'peek' 
    // function of InfosetSourceFromEventXMLReader
    // temporarily stores all 'next' events in 'savedEvents' prior
    // to permanently applying them once 'next' is called.
    //
    val nextEvent = ustate.peek
    handleDefaultable(nextEvent.node, ustate)
  }

  def handleDefaultable(isNode: DINode, ustate: UState): Unit = {
    isNode match {
      case e: DISimple if e.erd.isDefaultable => Assert.notYetImplemented("Unparser defaulting")
      case _ => // ok
    }
  }

}