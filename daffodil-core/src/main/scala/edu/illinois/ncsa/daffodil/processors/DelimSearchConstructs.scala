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

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind
import edu.illinois.ncsa.daffodil.dsom.StringValueAsLiteral
import edu.illinois.ncsa.daffodil.dsom.SingleCharacterLiteralES
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.dsom.DFDLEscapeScheme
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.dsom.ExpressionCompiler
import edu.illinois.ncsa.daffodil.dsom.SchemaComponent
import edu.illinois.ncsa.daffodil.dsom.R

object EscapeSchemeKind extends Enum {
  sealed abstract trait Type extends EnumValueType
  case object Character extends Type
  case object Block extends Type
  case object None extends Type
}

object EscapeScheme extends Logging {

  class EscapeSchemeObj(
    val escapeSchemeKind: EscapeSchemeKind.Type,
    val escapeCharacter: Option[CompiledExpression],
    val escapeEscapeCharacter: Option[CompiledExpression],
    val escapeBlockStart: String,
    val escapeBlockEnd: String) {

    private def evaluateEscapeEscapeCharacter(state: PState, context: ThrowsSDE): (VariableMap, String) = {
      val result = escapeEscapeCharacter match {
        case None => (state.variableMap, "")
        case Some(escEsc) => {
          val R(res, newVMap) = escEsc.evaluate(state.parentElement, state.variableMap, state)
          val l = new SingleCharacterLiteralES(res.toString, context)
          val resultEscEsc = l.cooked
          (newVMap, resultEscEsc)
        }
      }
      result
    }

    private def evaluateEscapeCharacter(state: PState, context: ThrowsSDE): (VariableMap, String) = {
      val result = escapeCharacter match {
        case None => (state.variableMap, "")
        case Some(esc) => {
          val R(res, newVMap) = esc.evaluate(state.parentElement, state.variableMap, state)
          val l = new SingleCharacterLiteralES(res.toString, context)
          val resultEsc = l.cooked
          (newVMap, resultEsc)
        }
      }
      result
    }

    def evaluate(state: PState, context: ThrowsSDE): (Option[PState], EvaluatedEscapeSchemeObj) = {
      val vmap = state.variableMap
      val result = escapeSchemeKind match {
        case EscapeSchemeKind.None => (None, new EvaluatedEscapeSchemeObj(EscapeSchemeKind.None, "", "", "", ""))
        case EscapeSchemeKind.Block => {
          val (finalVMap, resultEscEsc) = evaluateEscapeEscapeCharacter(state, context)
          val finalState = state.withVariables(finalVMap)
          (Some(finalState), new EvaluatedEscapeSchemeObj(EscapeSchemeKind.Block, "", resultEscEsc, escapeBlockStart, escapeBlockEnd))
        }
        case EscapeSchemeKind.Character => {
          val (postEscEscVMap, resultEscEsc) = evaluateEscapeEscapeCharacter(state, context)
          val newState = state.withVariables(postEscEscVMap)
          val (finalVMap, resultEsc) = evaluateEscapeCharacter(newState, context)
          val finalState = newState.withVariables(finalVMap)
          (Some(finalState), new EvaluatedEscapeSchemeObj(EscapeSchemeKind.Character, resultEsc, resultEscEsc, "", ""))
        }
      }
      result
    }

  }

  class EvaluatedEscapeSchemeObj(
    val escapeSchemeKind: EscapeSchemeKind.Type,
    val escapeCharacter: String,
    val escapeEscapeCharacter: String,
    val escapeBlockStart: String,
    val escapeBlockEnd: String) {
  }

  def getEscapeScheme(pEs: Option[DFDLEscapeScheme], context: SchemaComponent): EscapeSchemeObj = {
    val expressionCompiler = new ExpressionCompiler(context)
    var escapeSchemeKind: EscapeSchemeKind.Type = EscapeSchemeKind.None
    var escapeCharacter: Option[CompiledExpression] = None
    var escapeEscapeCharacter: Option[CompiledExpression] = None
    var escapeBlockStart = ""
    var escapeBlockEnd = ""

    pEs match {
      case None => escapeSchemeKind = EscapeSchemeKind.None
      case Some(obj) => {
        obj.escapeKind match {
          case EscapeKind.EscapeBlock => {
            escapeSchemeKind = EscapeSchemeKind.Block

            escapeEscapeCharacter = {
              val optEEC = obj.optionEscapeEscapeCharacter
              optEEC
            }
            escapeBlockStart = {
              val l = new StringValueAsLiteral(obj.escapeBlockStart, context)
              l.cooked
            }
            escapeBlockEnd = {
              val l = new StringValueAsLiteral(obj.escapeBlockEnd, context)
              l.cooked
            }
          }
          case EscapeKind.EscapeCharacter => {
            escapeSchemeKind = EscapeSchemeKind.Character
            escapeEscapeCharacter = {
              val optEEC = obj.optionEscapeEscapeCharacter
              optEEC
            }
            escapeCharacter = {
              val optEC = obj.optionEscapeCharacter
              optEC
            }
          }
          case _ => context.schemaDefinitionError("Unrecognized Escape Scheme!")
        }
      }
    }

    log(LogLevel.Debug, "EscapeSchemeKind: " + escapeSchemeKind)
    log(LogLevel.Debug, "\tEscapeCharacter: " + escapeCharacter)
    log(LogLevel.Debug, "\tEscapeEscapeCharacter: " + escapeEscapeCharacter)
    log(LogLevel.Debug, "\tEscapeBlockStart: " + escapeBlockStart)
    log(LogLevel.Debug, "\tEscapeBlockEnd: " + escapeBlockEnd)

    val result = new EscapeSchemeObj(escapeSchemeKind,
      escapeCharacter,
      escapeEscapeCharacter,
      escapeBlockStart,
      escapeBlockEnd)
    result
  }
}
