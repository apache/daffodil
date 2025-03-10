/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.unparsers.runtime1

import java.nio.charset.MalformedInputException
import java.nio.charset.UnmappableCharacterException

import org.apache.daffodil.lib.equality.ViewEqual
import org.apache.daffodil.lib.schema.annotation.props.gen.GenerateEscape
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.EscapeSchemeBlockUnparserHelper
import org.apache.daffodil.runtime1.processors.EscapeSchemeCharUnparserHelper
import org.apache.daffodil.runtime1.processors.EscapeSchemeUnparseEv
import org.apache.daffodil.runtime1.processors.dfa.CreateFieldDFA
import org.apache.daffodil.runtime1.processors.dfa.TextDelimitedUnparser
import org.apache.daffodil.runtime1.processors.unparsers._

sealed class StringDelimitedUnparser(
  override val context: ElementRuntimeData,
  escapeScheme: Maybe[EscapeSchemeUnparseEv],
  isDelimRequired: Boolean
) extends TextPrimUnparser {

  override def runtimeDependencies = Vector()

  val fieldDFA = CreateFieldDFA()
  val textUnparser = new TextDelimitedUnparser(context)

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
                val hasEscCharAsDelimiter =
                  terminatingMarkup.exists(d => d.lookingFor(0) =#= theScheme.ec)
                val thingsToEscape = (terminatingMarkup ++ scheme.lookingFor).toArray

                textUnparser.escapeCharacter(
                  dis,
                  fieldDFA,
                  thingsToEscape,
                  hasEscCharAsDelimiter,
                  theScheme.ec,
                  theScheme.eec,
                  state
                )
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

                val generateEscapeBlock =
                  (theScheme.generateEscapeBlock == GenerateEscape.Always) ||
                    valueString.startsWith(
                      theScheme.blockStart
                    ) || hasInscopeTerminatingDelimiters()

                val thingsToEscape = theScheme.lookingFor // blockEnd and extraEscapedCharacters

                textUnparser.escape(
                  dis,
                  fieldDFA,
                  thingsToEscape,
                  theScheme.blockEndDFA,
                  theScheme.eec,
                  theScheme.blockStart,
                  theScheme.blockEnd,
                  generateEscapeBlock,
                  state
                )
              }
            }

            result
          }
        } else valueString // No EscapeScheme

      val outStream = state.getDataOutputStream
      val nCharsWritten = outStream.putString(escapedValue, state)
      if (nCharsWritten != escapedValue.length)
        UE(
          state,
          "%s - Too many bits in field: IndexOutOfBounds. Insufficient space to write %s characters.",
          nom,
          escapedValue.length
        )
      Logger.log.debug(s"Ended at bit position ${outStream.relBitPos0b}")
    } catch {
      // Characters in infoset element cannot be encoded without error.
      //
      // This won't actually be thrown until encodingErrorPolicy='error' is
      // implemented.
      //
      case m: MalformedInputException => {
        UE(state, "%s - MalformedInputException: \n%s", nom, m.getMessage())
      }
      case u: UnmappableCharacterException => {
        UE(state, "%s - UnmappableCharacterException: \n%s", nom, u.getMessage())
      }
    }
  }

}

class LiteralNilDelimitedEndOfDataUnparser(
  erd: ElementRuntimeData,
  slForUnparserEv: NilStringLiteralForUnparserEv,
  isDelimRequired: Boolean
) extends StringDelimitedUnparser(erd, Nope, isDelimRequired) {

  final override def theString(ustate: UState) = slForUnparserEv.evaluate(ustate)

}
