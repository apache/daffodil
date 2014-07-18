package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.dsom.DFDLNewVariableInstance
import edu.illinois.ncsa.daffodil.dsom.AnnotatedSchemaComponent
import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.dsom.DFDLAssert
import java.nio.charset.Charset
import edu.illinois.ncsa.daffodil.processors.ExpressionEvaluatorBase
import edu.illinois.ncsa.daffodil.processors.TextReader
import edu.illinois.ncsa.daffodil.dsom.DFDLAssertionBase
import edu.illinois.ncsa.daffodil.processors.ExpressionEvaluationParser
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.processors.DFDLDelimParser
import edu.illinois.ncsa.daffodil.processors.DelimParseSuccess
import edu.illinois.ncsa.daffodil.processors.DelimParseFailure
import edu.illinois.ncsa.daffodil.processors.AssertionFailed
import edu.illinois.ncsa.daffodil.dsom.R
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Maybe._

class NewVariableInstanceStartParser(
  stmt: DFDLNewVariableInstance,
  gram: Gram,
  decl: AnnotatedSchemaComponent)
  extends PrimParser(gram, decl) {
  stmt.notYetImplemented("newVariableInstance")
  def parse(pstate: PState) = {
    stmt.notYetImplemented("newVariableInstance")
  }
}

class AssertBaseExpressionEvaluationParser(
  msg: String,
  discrim: Boolean, // are we a discriminator or not.
  decl: AnnotatedSchemaComponent,
  context: ExpressionEvaluatorBase)
  extends ExpressionEvaluationParser(context, decl) {

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

class AssertPatternPrimParser(
  eName: String,
  kindString: String,
  charset: Charset,
  d: ThreadLocal[DFDLDelimParser],
  decl: AnnotatedSchemaComponent,
  stmt: DFDLAssert,
  gram: Gram)
  extends PrimParser(gram, decl)
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

class DiscriminatorPatternPrimParser(
  testPattern: String,
  eName: String,
  kindString: String,
  charset: Charset,
  d: ThreadLocal[DFDLDelimParser],
  decl: AnnotatedSchemaComponent,
  stmt: DFDLAssertionBase,
  gram: Gram)
  extends PrimParser(gram, decl)
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