/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors.unparsers

import java.nio.charset.MalformedInputException
import java.nio.charset.UnmappableCharacterException

import edu.illinois.ncsa.daffodil.equality.ViewEqual
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeBlockUnparserHelper
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeCharUnparserHelper
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeUnparseEv
import edu.illinois.ncsa.daffodil.processors.dfa.CreateFieldDFA
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedUnparser
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.GenerateEscape
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.util.Maybe.One

sealed class StringDelimitedUnparser(erd: ElementRuntimeData,
  escapeScheme: Maybe[EscapeSchemeUnparseEv],
  isDelimRequired: Boolean)
  extends TextPrimUnparserObject(erd) {

  val fieldDFA = CreateFieldDFA()
  val textUnparser = new TextDelimitedUnparser(erd)

  protected def theString(state: UState) =
    state.currentInfosetNode.asSimple.dataValueAsString

  def unparse(state: UState): Unit = {

    val schemeOpt = if (escapeScheme.isDefined) One(escapeScheme.get.evaluate(state)) else Nope

    try {
      val valueString = theString(state)

      val escapedValue: String =
        if (schemeOpt.isDefined) {
          state.withUnparserDataInputStream { dis =>
            val terminatingMarkup = state.allTerminatingMarkup

            dis.reset(valueString, state)

            val scheme = schemeOpt.get

            val (result, _) = {
              if (scheme.isInstanceOf[EscapeSchemeCharUnparserHelper]) {
                val theScheme = scheme.asInstanceOf[EscapeSchemeCharUnparserHelper]
                val hasEscCharAsDelimiter = terminatingMarkup.exists(d => d.lookingFor.length == 1 && d.lookingFor(0) =#= theScheme.ec)
                val thingsToEscape = (terminatingMarkup ++ scheme.lookingFor).toArray

                textUnparser.escapeCharacter(dis, fieldDFA, thingsToEscape, hasEscCharAsDelimiter, theScheme.ec, theScheme.eec, state)
              } else {
                val theScheme = scheme.asInstanceOf[EscapeSchemeBlockUnparserHelper]

                def hasInscopeTerminatingDelimiters(): Boolean = {
                  // Need to do this so we can 'break' the loop early
                  //
                  for (d <- terminatingMarkup) {
                    if (valueString.contains(d.lookingFor)) return true
                  }
                  false
                }

                val generateEscapeBlock = (theScheme.generateEscapeBlock == GenerateEscape.Always) ||
                  valueString.startsWith(theScheme.blockStart) || hasInscopeTerminatingDelimiters()

                val thingsToEscape = theScheme.lookingFor // blockEnd and extraEscapedCharacters

                textUnparser.escape(dis, fieldDFA, thingsToEscape, theScheme.blockEndDFA,
                  theScheme.eec, theScheme.blockStart, theScheme.blockEnd,
                  generateEscapeBlock, state)
              }
            }

            result
          }
        } else valueString // No EscapeScheme

      val outStream = state.dataOutputStream
      val nCharsWritten = outStream.putString(escapedValue, state)
      if (nCharsWritten != escapedValue.length) UE(state, "%s - Too many bits in field: IndexOutOfBounds. Insufficient space to write %s characters.",
        nom, escapedValue.length)
      log(LogLevel.Debug, "Ended at bit position " + outStream.relBitPos0b)
    } catch {
      // Characters in infoset element cannot be encoded without error.
      //
      // This won't actually be thrown until encodingErrorPolicy='error' is
      // implemented.
      //
      case m: MalformedInputException => { UE(state, "%s - MalformedInputException: \n%s", nom, m.getMessage()) }
      case u: UnmappableCharacterException => { UE(state, "%s - UnmappableCharacterException: \n%s", nom, u.getMessage()) }
    }
  }

}

class LiteralNilDelimitedEndOfDataUnparser(
  erd: ElementRuntimeData,
  slForUnparserEv: NilStringLiteralForUnparserEv,
  isDelimRequired: Boolean)
  extends StringDelimitedUnparser(erd, Nope, isDelimRequired) {

  final override def theString(ustate: UState) = slForUnparserEv.evaluate(ustate)

}
