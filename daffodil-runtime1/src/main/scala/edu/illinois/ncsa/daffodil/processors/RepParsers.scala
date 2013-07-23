package edu.illinois.ncsa.daffodil.processors

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.api._
import java.nio._
import java.nio.charset._
import scala.collection.JavaConversions._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.debugger.Debugger

abstract class RepPrim(context: LocalElementBase, n: Long, r: => Gram) extends UnaryGram(context, r) {
  Assert.invariant(n > 0)
  val intN = n.toInt

  abstract class RepParser(context: LocalElementBase, baseName: String) extends Parser(context) {
    def checkN(pstate: PState, n: Long): Option[PState] = {
      if (n > DaffodilTunableParameters.occursCountMax) {
        // TODO: how can we go after bigger than max int bytes? We have 64-bit computers
        // after all....
        Some(PE(pstate, "Occurs count %s exceeds implementation maximum of %s.", n, DaffodilTunableParameters.occursCountMax))
      } else None
    }

    val rParser = r.parser

    final def parse(pstate: PState): PState = {
      checkN(pstate, n).map { perr => return perr }
      val res = parseAllRepeats(pstate)
      res
    }

    protected def parseAllRepeats(pstate: PState): PState

    override def toString = "Rep" + baseName + "(" + rParser.toString + ")"

    def toBriefXML(depthLimit: Int = -1): String = {
      if (depthLimit == 0) "..." else
        "<Rep" + baseName + ">" + rParser.toBriefXML(depthLimit - 1) +
          "</Rep" + baseName + ">"
    }
  }
}

abstract class Rep3Arg(f: (LocalElementBase, Long, => Gram) => Gram) {
  def apply(context: LocalElementBase, n: Long, rr: => Gram) = {
    val r = rr
    if (n == 0 || r.isEmpty) EmptyGram
    else f(context, n, r)
  }
}

abstract class Rep2Arg(f: (LocalElementBase, => Gram) => Gram) {
  def apply(context: LocalElementBase, r: => Gram) = {
    val rr = r
    if (rr.isEmpty) EmptyGram
    else f(context, r)
  }
}

object RepExactlyN extends Rep3Arg(new RepExactlyNPrim(_, _, _))

object RepAtMostTotalN extends Rep3Arg(new RepAtMostTotalNPrim(_, _, _))

object RepUnbounded extends Rep2Arg(new RepUnboundedPrim(_, _))

object RepExactlyTotalN extends Rep3Arg(new RepExactlyTotalNPrim(_, _, _))

object RepAtMostOccursCount extends Rep3Arg(new RepAtMostOccursCountPrim(_, _, _))

object RepExactlyTotalOccursCount extends Rep2Arg(new RepExactlyTotalOccursCountPrim(_, _))

class RepExactlyNPrim(context: LocalElementBase, n: Long, r: => Gram) extends RepPrim(context, n, r) {

  // Since this is Exactly N, there is no new point of uncertainty considerations here.
  def parser = new RepParser(context, "ExactlyN") {
    def parseAllRepeats(pstate: PState): PState = {
      var pResult = pstate // TODO: find perfect monad functional programming idiom to eliminate this var
      1 to intN foreach { _ =>
        {
          Debugger.beforeRepetition(pResult, this)
          val pNext = rParser.parse1(pResult, context)
          Debugger.afterRepetition(pResult, pNext, this)
          if (pNext.status != Success) return pNext // they all must succeed, otherwise we fail here.
          pResult = pNext.moveOverOneArrayIndexOnly
        }
      }
      pResult
    }
  }

  def unparser = new RepExactlyNUnparser(context, n, r)
}

class RepAtMostTotalNPrim(context: LocalElementBase, n: Long, r: => Gram) extends RepPrim(context, n, r) {

  def parser = new RepParser(context, "AtMostTotalN") {

    def parseAllRepeats(pstate: PState): PState = {
      var pResult = pstate
      while (pResult.arrayIndexStack.head <= intN) {
        // Since each one could fail, each is a new point of uncertainty.
        val newpou = pResult.withNewPointOfUncertainty
        // 
        // save the state of the infoset
        //
        val cloneNode = newpou.captureInfosetElementState

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
          // backout any element appended as part of this attempt.
          //
          newpou.restoreInfosetElementState(cloneNode)
          return pResult // success at prior state. 
        }
        pResult = pNext.moveOverOneArrayIndexOnly.withRestoredPointOfUncertainty
      }
      pResult
    }
  }

  def unparser = DoNothingUnparser(context) // all elements will already have been output
}

/**
 * This object is so that we can share the iteration idioms between situations
 * where we know N statically, and where dynamic evaluation computes N.
 *
 * In these cases, there are no new points of uncertainty because computed or
 * otherwise, we know N.
 */
object Rep {
  def loopExactlyTotalN(intN: Int, rParser: Parser, pstate: PState, context: SchemaComponent, iParser: Parser): PState = {
    var pResult = pstate
    while (pResult.arrayPos <= intN) {
      Debugger.beforeRepetition(pResult, iParser)
      val pNext = rParser.parse1(pResult, context)
      Debugger.afterRepetition(pResult, pNext, iParser)
      if (pNext.status != Success) return pNext // fail if we don't get them all 
      pResult = pNext.moveOverOneArrayIndexOnly
    }
    pResult
  }
}

class RepExactlyTotalNPrim(context: LocalElementBase, n: Long, r: => Gram) extends RepPrim(context, n, r) {

  def parser = new RepParser(context, "ExactlyTotalN") {
    def parseAllRepeats(pstate: PState): PState = {
      Rep.loopExactlyTotalN(intN, rParser, pstate, context, this)
    }
  }

  def unparser = DoNothingUnparser(context) // all elements will already have been output
}

class RepUnboundedPrim(e: LocalElementBase, r: => Gram) extends RepPrim(e, 1, r) {

  def parser = new RepParser(e, "Unbounded") {

    def parseAllRepeats(pstate: PState): PState = {

      var pResult = pstate
      while (pResult.status == Success) {

        if ((e.occursCountKind == OccursCountKind.Implicit) &&
            (e.maxOccurs == -1)) {
          if (pResult.arrayPos-1 <= e.minOccurs) {
            // Is required element
            // Somehow need to return that this element is required
          }
        }

        val cloneNode = pResult.captureInfosetElementState
        //
        // Every parse is a new point of uncertainty.
        val newpou = pResult.withNewPointOfUncertainty
        Debugger.beforeRepetition(newpou, this)
        val pNext = rParser.parse1(newpou, e)
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
          pResult.restoreInfosetElementState(cloneNode)
          log(LogLevel.Debug, "Failure suppressed. This is normal termination of a occursCountKind='parsed' array.")
          return pResult // note that it has the prior point of uncertainty. No restore needed.
        }
        // Success
        // Need to check for forward progress
        if (pResult.bitPos == pNext.bitPos) {
          return PE(pNext,
            "RepUnbounded - No forward progress at byte %s. Attempt to parse %s " +
              "succeeded but consumed no data.\nPlease re-examine your schema to correct this infinite loop.",
            pResult.bytePos, e.prettyName)
        }
        pResult = pNext.moveOverOneArrayIndexOnly.withRestoredPointOfUncertainty // point of uncertainty has been resolved.

      }
      Assert.invariantFailed("Unbounded loop terminated wrong")
    }
  }

  def unparser = new RepUnboundedUnparser(e, r)
}

case class OccursCountExpression(e: ElementBase)
  extends Terminal(e, true) {
  val pseudoElement = Infoset.newElement(e, e.isHidden)
  val exprText = e.occursCount.prettyExpr

  def parser = new Parser(e) with WithParseErrorThrowing {
    def parse(pstate: PState): PState = withParseErrorThrowing(pstate) {
      //
      // Because the occurs count expression will be written as if we were already in a child node
      // (e.g., ../countField where countField is a peer) we have to make a fake node, and attach it
      // just for purposes of having the right relative path stuff here.

      val priorElement = pstate.parentElement
      priorElement.addElement(pseudoElement)
      val res = try {
        val R(oc, newVMap) = e.occursCount.evaluate(pseudoElement, pstate.variableMap, pstate)
        val postEvalState = pstate.withVariables(newVMap)
        priorElement.removeContent(pseudoElement) // TODO: faster way? This might involve searching. We should keep the index.
        val ocLong = oc.asInstanceOf[Long]
        if (ocLong < 0 ||
          ocLong > DaffodilTunableParameters.occursCountMax) {
          return PE(postEvalState, "Evaluation of occursCount expression %s returned out of range value %s.", exprText, ocLong)
        }
        postEvalState.setOccursCount(ocLong)
      } catch {
        case u: UnsuppressableException => throw u
        case e: Exception =>
          PE(pstate, "Evaluation of occursCount expression %s threw exception %s", exprText, e)
      }
      res
    }

    override def toString = toBriefXML() // "OccursCount(" + e.occursCount.prettyExpr + ")"

    def toBriefXML(depthLimit: Int = -1) = {
      "<OccursCount>" + e.occursCount.prettyExpr + "</OccursCount>"
    }
  }

  def unparser = new DummyUnparser(e)
}

class RepAtMostOccursCountPrim(e: LocalElementBase, n: Long, r: => Gram)
  extends RepPrim(e, n, r) {

  def parser = new RepParser(e, "AtMostOccursCount") {
    def parseAllRepeats(pstate: PState): PState = {
      // repeat either n times, or occursCount times if that's less than n.
      val n = math.min(pstate.occursCount, e.minOccurs)
      Rep.loopExactlyTotalN(intN, rParser, pstate, e, this)
    }
  }

  def unparser = new DummyUnparser(context)
}

class RepExactlyTotalOccursCountPrim(e: LocalElementBase, r: => Gram) extends RepPrim(e, 1, r) {

  def parser = new RepParser(e, "ExactlyTotalOccursCount") {
    def parseAllRepeats(pstate: PState): PState = {
      val ocInt = pstate.occursCount.toInt
      Rep.loopExactlyTotalN(ocInt, rParser, pstate, e, this)
    }
  }

  def unparser = new DummyUnparser(context)
}
