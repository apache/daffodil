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

import edu.illinois.ncsa.daffodil.dsom.DFDLEscapeScheme
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.EntityReplacer
import edu.illinois.ncsa.daffodil.dsom.StringValueAsLiteral
import edu.illinois.ncsa.daffodil.dsom.SingleCharacterLiteral
import edu.illinois.ncsa.daffodil.dsom.SingleCharacterLiteralES
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE

object EscapeSchemeKind extends Enum {
  sealed abstract trait Type extends EnumValueType
  case object Character extends Type
  case object Block extends Type
  case object None extends Type
}

object EscapeScheme extends Logging {

  class EscapeSchemeObj {
    var escapeSchemeKind: EscapeSchemeKind.Type = EscapeSchemeKind.None
    var escapeCharacter = ""
    var escapeEscapeCharacter = ""
    var escapeBlockStart = ""
    var escapeBlockEnd = ""
  }

  def getEscapeScheme(pEs: Option[DFDLEscapeScheme], context: ThrowsSDE): EscapeSchemeObj = {
    var escapeSchemeKind: EscapeSchemeKind.Type = EscapeSchemeKind.None
    var escapeCharacter = ""
    var escapeEscapeCharacter = ""
    var escapeBlockStart = ""
    var escapeBlockEnd = ""

    pEs match {
      case None => escapeSchemeKind = EscapeSchemeKind.None
      case Some(obj) => {
        obj.escapeKind match {
          case EscapeKind.EscapeBlock => {
            escapeSchemeKind = EscapeSchemeKind.Block
            escapeEscapeCharacter = {
              val optKEEC = obj.knownEscapeEscapeCharacter
              val eec = optKEEC.getOrElse("")
              val l = new SingleCharacterLiteralES(eec, context)
              l.cooked
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
              val l = new SingleCharacterLiteralES(obj.escapeEscapeCharacterRaw.value, context)
              l.cooked
            }
            escapeCharacter = {
              val l = new SingleCharacterLiteralES(obj.escapeCharacterRaw.value, context)
              l.cooked
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

    val result = new EscapeSchemeObj
    result.escapeSchemeKind = escapeSchemeKind
    result.escapeCharacter = escapeCharacter
    result.escapeEscapeCharacter = escapeEscapeCharacter
    result.escapeBlockStart = escapeBlockStart
    result.escapeBlockEnd = escapeBlockEnd
    result
  }
}
