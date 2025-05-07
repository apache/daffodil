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

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.parsers.DelimiterTextType
import org.apache.daffodil.runtime1.processors.unparsers._

class DelimiterTextUnparser(
  override val context: TermRuntimeData,
  delimiterType: DelimiterTextType.Type
) extends TextPrimUnparser {

  private def erd = context

  override def runtimeDependencies = Vector()

  override lazy val nom = {
    if (delimiterType == DelimiterTextType.Initiator) "InitiatorUnparser"
    else if (delimiterType == DelimiterTextType.Separator) "SeparatorUnparser"
    else "TerminatorUnparser"
  }

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..."
    else "<" + nom + " />"
  }

  def unparse(state: UState): Unit = {

    Logger.log.debug(
      s"Unparsing starting at bit position: ${state.getDataOutputStream.maybeAbsBitPos0b}"
    )

    val localDelimNode = state.localDelimiters

    val delimDFAs = {
      if (delimiterType == DelimiterTextType.Initiator) localDelimNode.initiator
      else if (delimiterType == DelimiterTextType.Separator) localDelimNode.separator
      else localDelimNode.terminator
    }

    if (delimDFAs.length == 0)
      Assert.invariantFailed(
        "Expected a delimiter of type " + delimiterType + " on the stack, but was not found."
      )

    val delimDFA = delimDFAs(0)

    try {
      val valueString = delimDFA.unparseValue

      val outStream = state.getDataOutputStream
      val nCharsWritten = outStream.putString(valueString, state)
      if (nCharsWritten != valueString.length)
        UE(
          state,
          "%s - Too many bits in delimiter: IndexOutOfBounds. Insufficient space to write delimiter '%s'.",
          nom,
          Misc.remapStringToVisibleGlyphs(valueString)
        )
    } catch {
      // Characters in infoset element cannot be encoded without error.
      //
      // This won't actually be thrown until encodingErrorPolicy='error' is
      // implemented.
      //
      case m: MalformedInputException => {
        UnparseError(
          One(erd.schemaFileLocation),
          One(state.currentLocation),
          "%s - MalformedInputException: \n%s",
          nom,
          m.getMessage()
        )
      }
      case u: UnmappableCharacterException => {
        UnparseError(
          One(erd.schemaFileLocation),
          One(state.currentLocation),
          "%s - UnmappableCharacterException: \n%s",
          nom,
          u.getMessage()
        )
      }
    }
  }

}
