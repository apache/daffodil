package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.dsom.DFDLNewVariableInstance
import edu.illinois.ncsa.daffodil.dsom.AnnotatedSchemaComponent
import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.dsom.DFDLAssert
import java.nio.charset.Charset
import edu.illinois.ncsa.daffodil.processors.ExpressionEvaluatorBase
import edu.illinois.ncsa.daffodil.processors.TextReader
import edu.illinois.ncsa.daffodil.dsom.DFDLAssertionBase
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.processors.DFDLDelimParser
import edu.illinois.ncsa.daffodil.processors.DelimParseSuccess
import edu.illinois.ncsa.daffodil.processors.DelimParseFailure
import edu.illinois.ncsa.daffodil.processors.AssertionFailed
import edu.illinois.ncsa.daffodil.dsom.R
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.dsom.DFDLSetVariable
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.processors.InputValueCalc
import edu.illinois.ncsa.daffodil.processors.WithParseErrorThrowing
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.Parser
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData

/**
 * Common parser base class for any parser that evaluates an expression.
 */
abstract class ExpressionEvaluationParser(expr: CompiledExpression, rd: RuntimeData)
  extends Parser(rd) with WithParseErrorThrowing {

  override def toBriefXML(depthLimit: Int = -1) = {
    "<" + rd.prettyName + ">" + expr.prettyExpr + "</" + rd.prettyName + ">"
  }

  def eval(start: PState) = {
    val currentElement = start.parentElement
    val R(res, newVMap) =
      expr.evaluate(currentElement, start.variableMap, start)
    // val result = res.toString // Everything in JDOM is a string!
    R(res, newVMap)
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
        val currentElement = start.parentElement
        val R(res, newVMap) = eval(start)

        currentElement.setDataValue(res.toString)
        val postState = start.withVariables(newVMap) // inputValueCalc consumes nothing. Just creates a value.
        postState
      }
    }
}

class SetVariableParser(expr: CompiledExpression, decl: RuntimeData, name: String)
  extends ExpressionEvaluationParser(expr, decl) {

  def parse(start: PState): PState =
    // withLoggingLevel(LogLevel.Info) 
    {
      withParseErrorThrowing(start) {
        log(LogLevel.Debug, "This is %s", toString)
        val R(res, newVMap) = eval(start)
        val newVMap2 = newVMap.setVariable(name, res, decl)
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
        val R(res, newVMap) = eval(start)
        val testResult = res.asInstanceOf[Boolean]
        val postState = start.withVariables(newVMap)
        if (testResult) {
          postState.withDiscriminator(discrim)
        } else {
          // The assertion failed. Prepare a failure message etc. in case backtracking ultimately fails from here.
          val diag = new AssertionFailed(decl, postState, msg)
          postState.failed(diag)
        }
      }
    }
}

class AssertPatternParser(
  eName: String,
  kindString: String,
  charset: Charset,
  d: ThreadLocal[DFDLDelimParser],
  decl: RuntimeData,
  stmt: DFDLAssert)
  extends PrimParser(decl)
  with TextReader {

  val testPattern = stmt.testTxt
  val csName = charset.name()

  override def toBriefXML(depthLimit: Int = -1) = {
    "<" + kindString + ">" + testPattern + "</" + kindString + ">"
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

        val reader = getReader(charset, start.bitPos, lastState)

        val result = d.get.parseInputPatterned(testPattern, reader, start)

        val postState = result match {
          case s: DelimParseSuccess => {
            val endBitPos = lastState.bitPos + s.numBits
            log(LogLevel.Debug, "Assert Pattern success for testPattern %s", testPattern)
            start
          }
          case f: DelimParseFailure => {
            log(LogLevel.Debug, "Assert Pattern fail for testPattern %s\nDetails: %s", testPattern, f.msg)
            val diag = new AssertionFailed(decl, start, stmt.message, One(f.msg))
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
  charset: Charset,
  d: ThreadLocal[DFDLDelimParser],
  decl: RuntimeData,
  stmt: DFDLAssertionBase,
  gram: Gram)
  extends PrimParser(decl)
  with TextReader {

  val csName = charset.name()

  override def toBriefXML(depthLimit: Int = -1) = {
    "<" + kindString + ">" + testPattern + "</" + kindString + ">"
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

        val reader = getReader(charset, start.bitPos, lastState)

        val result = d.get.parseInputPatterned(testPattern, reader, start)

        // Only want to set the discriminator if it is true
        // we do not want to modify it unless it's true
        val finalState = result match {
          case s: DelimParseSuccess => start.withDiscriminator(true)
          case f: DelimParseFailure => {
            val diag = new AssertionFailed(decl, start, stmt.message, One(f.msg))
            start.failed(diag)
          }
        }
        finalState
      }
    }
}