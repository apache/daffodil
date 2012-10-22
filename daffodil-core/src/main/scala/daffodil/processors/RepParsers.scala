package daffodil.processors

import org.jdom._
import daffodil.xml._
import daffodil.xml._
import daffodil.processors._
import daffodil.grammar._
import daffodil.exceptions.Assert
import daffodil.schema.annotation.props._
import daffodil.dsom._
import daffodil.compiler._
import daffodil.api._
import java.nio._
import java.nio.charset._
import stringsearch._
import scala.collection.JavaConversions._
import scala.util.logging.ConsoleLogger
import stringsearch.DelimSearcherV3._
import scala.collection.mutable.Queue
import scala.util.matching.Regex
import stringsearch.constructs._
import stringsearch.constructs.EscapeScheme._
import daffodil.util._
import daffodil.exceptions.ThrowsSDE
import java.io.ByteArrayInputStream
import scala.collection.mutable.Stack
import daffodil.exceptions.UnsuppressableException
import daffodil.debugger.Debugger

abstract class RepPrim(context : LocalElementBase, n : Long, r : => Gram) extends UnaryGram(context, r) {
  Assert.invariant(n > 0)
  val intN = n.toInt

  abstract class RepParser(context : LocalElementBase, baseName : String) extends Parser(context) {
    def checkN(pstate : PState, n : Long) : Option[PState] = {
      if (n > Compiler.occursCountMax) {
        // TODO: how can we go after bigger than max int bytes? We have 64-bit computers
        // after all....
        Some(PE(pstate, "Occurs count %s exceeds implementation maximum of %s.", n, Compiler.occursCountMax))
      } else None
    }

    val rParser = r.parser

    final def parse(pstate : PState) : PState = {
      checkN(pstate, n).map { perr => return perr }
      val res = parseAllRepeats(pstate)
      res
    }

    protected def parseAllRepeats(pstate : PState) : PState

    override def toString = "Rep" + baseName + "(" + rParser.toString + ")"

    def toBriefXML(depthLimit : Int = -1) : String = {
      if (depthLimit == 0) "..." else
        "<Rep" + baseName + ">" + rParser.toBriefXML(depthLimit - 1) +
          "</Rep" + baseName + ">"
    }
  }
}

class RepExactlyNPrim(context : LocalElementBase, n : Long, r : => Gram) extends RepPrim(context, n, r) {

  // Since this is Exactly N, there is no new point of uncertainty considerations here.
  def parser = new RepParser(context, "ExactlyN") {
    def parseAllRepeats(pstate : PState) : PState = {
      var pResult = pstate // TODO: find perfect monad functional programming idiom to eliminate this var
      1 to intN foreach { _ =>
        {
          Debugger.beforeRepetition(pResult, this)
          val pNext = rParser.parse1(pResult, context)
          Debugger.afterRepetition(pResult, pNext, this)
          if (pNext.status != Success) return pNext // they all must succeed, otherwise we fail here.
          pResult = pNext
        }
      }
      pResult
    }
  }

  def unparser = new RepExactlyNUnparser(context, n, r)
}

class RepAtMostTotalNPrim(context : LocalElementBase, n : Long, r : => Gram) extends RepPrim(context, n, r) {

  def parser = new RepParser(context, "AtMostTotalN") {

    def parseAllRepeats(pstate : PState) : PState = {
      var pResult = pstate
      while (pResult.arrayIndexStack.head <= intN) {
        // Since each one could fail, each is a new point of uncertainty.
        val newpou = pResult.withNewPointOfUncertainty
        Debugger.beforeRepetition(newpou, this)
        val pNext = rParser.parse1(newpou, context)
        Debugger.afterRepetition(newpou, pNext, this)
        if (pNext.status != Success) return pResult // success at prior state. 
        pResult = pNext.withRestoredPointOfUncertainty
      }
      pResult
    }
  }

  def unparser = DoNothingUnparser(context) // all elements will already have been output
}

/**
 * This object is so that we can share the iteration idioms between situations
 * where we know N statically, and where dynamic evaluation computes N.
 * <p>
 * In these cases, there are no new points of uncertainty because computed or
 * otherwise, we know N.
 */
object Rep {
  def loopExactlyTotalN(intN : Int, rParser : Parser, pstate : PState, context : SchemaComponent, iParser : Parser) : PState = {
    var pResult = pstate
    while (pResult.arrayPos <= intN) {
      Debugger.beforeRepetition(pResult, iParser)
      val pNext = rParser.parse1(pResult, context)
      Debugger.afterRepetition(pResult, pNext, iParser)
      if (pNext.status != Success) return pNext // fail if we don't get them all 
      pResult = pNext
    }
    pResult
  }
}

class RepExactlyTotalNPrim(context : LocalElementBase, n : Long, r : => Gram) extends RepPrim(context, n, r) {

  def parser = new RepParser(context, "ExactlyTotalN") {
    def parseAllRepeats(pstate : PState) : PState = {
      Rep.loopExactlyTotalN(intN, rParser, pstate, context, this)
    }
  }

  def unparser = DoNothingUnparser(context) // all elements will already have been output
}

class RepUnboundedPrim(context : LocalElementBase, r : => Gram) extends RepPrim(context, 1, r) {

  def parser = new RepParser(context, "Unbounded") {

    def parseAllRepeats(pstate : PState) : PState = {

      var pResult = pstate
      while (pResult.status == Success) {

        val cloneNode = pResult.captureJDOM
        //
        // Every parse is a new point of uncertainty.
        val newpou = pResult.withNewPointOfUncertainty
        Debugger.beforeRepetition(newpou, this)
        val pNext = rParser.parse1(newpou, context)
        Debugger.afterRepetition(newpou, pNext, this)
        if (pNext.status != Success) {
          // 
          // Did not succeed
          // 
          // Was a discriminator set?
          // 
          if (pNext.discriminator == true) {
            // we fail the whole RepUnbounded, because there was a discriminator set 
            // before the failure.
            return pNext.withRestoredPointOfUncertainty
          }
          // 
          // no discriminator, so suppress the failure. Loop terminated with prior element.
          //
          pResult.restoreJDOM(cloneNode)
          log(Debug("Failure suppressed."))
          return pResult // note that it has the prior point of uncertainty. No restore needed.
        }
        // Success
        // Need to check for forward progress
        if (pResult.bitPos == pNext.bitPos) {
          return PE(pNext,
            "RepUnbounded - No forward progress at byte %s. Attempt to parse %s " +
              "succeeded but consumed no data.\nPlease re-examine your schema to correct this infinite loop.",
            pResult.bytePos, context.prettyName)
        }
        pResult = pNext.withRestoredPointOfUncertainty // point of uncertainty has been resolved.

      }
      Assert.invariantFailed("Unbounded loop terminated wrong")
    }
  }

  def unparser = new RepUnboundedUnparser(context, r)
}

case class OccursCountExpression(e : ElementBase)
  extends Terminal(e, true) {
  val pseudoElement = new org.jdom.Element(e.name, e.targetNamespacePrefix, e.targetNamespace)

  def parser = new Parser(e) with WithParseErrorThrowing {
    def parse(pstate : PState) : PState = withParseErrorThrowing(pstate) {
      val exprText = e.occursCount.prettyExpr
      //
      // Because the occurs count expression will be written as if we were already in a child node
      // (e.g., ../countField where countField is a peer) we have to make a fake node, and attach it
      // just for purposes of having the right relative path stuff here.

      val priorElement = pstate.parentForAddContent
      priorElement.addContent(pseudoElement)
      val res = try {
        val R(oc, newVMap) = e.occursCount.evaluate(pseudoElement, pstate.variableMap, pstate)
        val postEvalState = pstate.withVariables(newVMap)
        priorElement.removeContent(pseudoElement) // TODO: faster way? This might involve searching. We should keep the index.
        val ocLong = oc.asInstanceOf[Long]
        if (ocLong < 0 ||
          ocLong > Compiler.occursCountMax) {
          return PE(postEvalState, "Evaluation of occursCount expression %s returned out of range value %s.", exprText, ocLong)
        }
        postEvalState.setOccursCount(ocLong)
      } catch {
        case u : UnsuppressableException => throw u
        case e : Exception =>
          PE(pstate, "Evaluation of occursCount expression %s threw exception %s", exprText, e)
      }
      res
    }

    override def toString = toBriefXML() // "OccursCount(" + e.occursCount.prettyExpr + ")"

    def toBriefXML(depthLimit : Int = -1) = {
      "<OccursCount>" + e.occursCount.prettyExpr + "</OccursCount>"
    }
  }

  def unparser = new DummyUnparser(e)
}

class RepAtMostOccursCountPrim(e : LocalElementBase, n : Long, r : => Gram) extends RepPrim(e, n, r) {

  def parser = new RepParser(e, "AtMostOccursCount") {
    def parseAllRepeats(pstate : PState) : PState = {
      // repeat either n times, or occursCount times if that's less than n.
      val n = math.min(pstate.occursCount, e.minOccurs)
      Rep.loopExactlyTotalN(intN, rParser, pstate, e, this)
    }
  }

  def unparser = new DummyUnparser(context)
}

class RepExactlyTotalOccursCountPrim(e : LocalElementBase, r : => Gram) extends RepPrim(e, 1, r) {

  def parser = new RepParser(e, "ExactlyTotalOccursCount") {
    def parseAllRepeats(pstate : PState) : PState = {
      val ocInt = pstate.occursCount.toInt
      Rep.loopExactlyTotalN(ocInt, rParser, pstate, e, this)
    }
  }

  def unparser = new DummyUnparser(context)
}