/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.processors.charset._
import java.nio.charset.Charset
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.Debug
import edu.illinois.ncsa.daffodil.util.PreSerialization
import edu.illinois.ncsa.daffodil.dsom.RuntimeEncodingMixin

/**
 * Common parser base class for any parser that evaluates an expression.
 */
abstract class ExpressionEvaluationParser(
  expr: CompiledExpression,
  rd: RuntimeData)
  extends Parser(rd) with WithParseErrorThrowing {

  override lazy val childProcessors = Nil

  /**
   * Returns a pair. Modifies the PState
   */
  protected def eval(start: PState): (Any, VariableMap) = {
    expr.evaluate(start)
  }
}

class IVCParser(expr: CompiledExpression, e: ElementRuntimeData)
  extends ExpressionEvaluationParser(expr, e) {
  Assert.invariant(e.isSimpleType)

  def parse(start: PState): PState =
    // withLoggingLevel(LogLevel.Info) 
    {
      withParseErrorThrowing(start) {
        log(LogLevel.Debug, "This is %s", toString)
        val currentElement: InfosetSimpleElement = start.simpleElement
        val (res, newVMap) = eval(start)
        if (start.status != Success) return start
        currentElement.setDataValue(res)
        val postState = start.withVariables(newVMap) // inputValueCalc consumes nothing. Just creates a value.
        postState
      }
    }
}

class SetVariableParser(expr: CompiledExpression, decl: VariableRuntimeData)
  extends ExpressionEvaluationParser(expr, decl) {

  def parse(start: PState): PState =
    // withLoggingLevel(LogLevel.Info) 
    {
      withParseErrorThrowing(start) {
        log(Debug("This is %s", toString)) // important. Don't toString unless we have to log. 
        val (res, newVMap) = eval(start)
        res match {
          case ps: PState => return ps;
          case _ => /*fall through*/ }
        if (start.status.isInstanceOf[Failure]) return start
        val newVMap2 = newVMap.setVariable(decl.globalQName, res, decl)
        val postState = start.withVariables(newVMap2)
        postState
      }
    }
}

class NewVariableInstanceStartParser(
  decl: RuntimeData)
  extends PrimParser(decl) {
  decl.notYetImplemented("newVariableInstance")
  def parse(pstate: PState) = {
    decl.notYetImplemented("newVariableInstance")
  }
}

class NewVariableInstanceEndParser(
  decl: RuntimeData)
  extends PrimParser(decl) {
  decl.notYetImplemented("newVariableInstance")
  def parse(pstate: PState) = {
    decl.notYetImplemented("newVariableInstance")
  }
}

class AssertExpressionEvaluationParser(
  msg: String,
  discrim: Boolean, // are we a discriminator or not.
  decl: RuntimeData,
  expr: CompiledExpression)
  extends ExpressionEvaluationParser(expr, decl) {

  def parse(start: PState): PState =
    // withLoggingLevel(LogLevel.Info) 
    {
      withParseErrorThrowing(start) {
        log(LogLevel.Debug, "This is %s", toString)
        //
        // This now informs us of the success/failure of the expression
        // evaluation via side-effect on the start state passed here.
        //
        val (res, newVMap) = eval(start)
        //
        // a PE during evaluation of an assertion is a PE
        //
        // Removed this assert check because eval now side-effects start to
        // contain the result status.
        // Assert.invariant(!start.status.isInstanceOf[Failure])
        //
        // Assert.invariant(res != null)
        start.status match {
          case Success => // ok
          case f: Failure => return start
        }
        val testResult = res.asInstanceOf[Boolean]
        val postState = start.withVariables(newVMap)
        if (testResult) {
          postState.withDiscriminator(discrim)
        } else {
          // The assertion failed. Prepare a failure message etc. in case backtracking ultimately fails from here.
          val diag = new AssertionFailed(decl.schemaFileLocation, postState, msg)
          postState.failed(diag)
        }
      }
    }
}

class AssertPatternParser(
  eName: String,
  kindString: String,
  override val encodingInfo: EncodingInfo,
  rd: RuntimeData,
  testPattern: String,
  message: String)
  extends PrimParser(rd)
  with TextReader with RuntimeEncodingMixin {

  override def toBriefXML(depthLimit: Int = -1) = {
    "<" + kindString + ">" + testPattern + "</" + kindString + ">"
  }

  @transient lazy val d = new ThreadLocal[DFDLDelimParser] {
    override def initialValue() = {
      new DFDLDelimParser(rd, encodingInfo)
    }
  }

  def parse(start: PState): PState =
    // withLoggingLevel(LogLevel.Info) 
    {
      withParseErrorThrowing(start) {
        val lastState = start // .withLastState
        val bytePos = (lastState.bitPos >> 3).toInt
        log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, lastState.bitPos)
        log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

        log(LogLevel.Debug, "%s - Looking for testPattern = %s", eName, testPattern)

        if (lastState.bitPos % 8 != 0) {
          return PE(lastState, "%s - not byte aligned.", eName)
        }

        log(LogLevel.Debug, "Retrieving reader")

        val reader = getReader(dcharset.charset, start.bitPos, lastState)

        val result = d.get.parseInputPatterned(testPattern, reader, start)

        val postState = result match {
          case s: DelimParseSuccess => {
            val endBitPos = lastState.bitPos + s.numBits
            log(LogLevel.Debug, "Assert Pattern success for testPattern %s", testPattern)
            start
          }
          case f: DelimParseFailure => {
            log(LogLevel.Debug, "Assert Pattern fail for testPattern %s\nDetails: %s", testPattern, f.msg)
            val diag = new AssertionFailed(rd.schemaFileLocation, start, message, One(f.msg))
            start.failed(diag)
          }
        }
        postState
      }
    }
}

class DiscriminatorPatternParser(
  testPattern: String,
  eName: String,
  kindString: String,
  override val encodingInfo: EncodingInfo,
  rd: RuntimeData,
  message: String)
  extends PrimParser(rd)
  with TextReader {

  override def toBriefXML(depthLimit: Int = -1) = {
    "<" + kindString + ">" + testPattern + "</" + kindString + ">"
  }

  @transient lazy val d = new ThreadLocal[DFDLDelimParser] {
    override def initialValue() = {
      new DFDLDelimParser(rd, encodingInfo)
    }
  }

  def parse(start: PState): PState =
    // withLoggingLevel(LogLevel.Info) 
    {
      withParseErrorThrowing(start) {
        val lastState = start // .withLastState
        val bytePos = (lastState.bitPos >> 3).toInt
        log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, lastState.bitPos)
        log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

        log(LogLevel.Debug, "%s - Looking for testPattern = %s", eName, testPattern)

        if (lastState.bitPos % 8 != 0) {
          return PE(lastState, "%s - not byte aligned.", eName)
        }

        log(LogLevel.Debug, "Retrieving reader")

        val reader = getReader(dcharset.charset, start.bitPos, lastState)

        val result = d.get.parseInputPatterned(testPattern, reader, start)

        // Only want to set the discriminator if it is true
        // we do not want to modify it unless it's true
        val finalState = result match {
          case s: DelimParseSuccess => start.withDiscriminator(true)
          case f: DelimParseFailure => {
            val diag = new AssertionFailed(rd.schemaFileLocation, start, message, One(f.msg))
            start.failed(diag)
          }
        }
        finalState
      }
    }
}