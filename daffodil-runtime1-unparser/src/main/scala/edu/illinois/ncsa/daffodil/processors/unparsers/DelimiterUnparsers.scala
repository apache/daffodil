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

import edu.illinois.ncsa.daffodil.processors.TermRuntimeData
import edu.illinois.ncsa.daffodil.processors.parsers.DelimiterTextType
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.util.LogLevel
import java.nio.charset.MalformedInputException
import java.nio.charset.UnmappableCharacterException
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.exceptions.Assert

class DelimiterTextUnparser(erd: TermRuntimeData, delimiterType: DelimiterTextType.Type)
  extends TextPrimUnparserObject(erd) {

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

    log(LogLevel.Debug, "Unparsing starting at bit position: %s", state.dataOutputStream.maybeAbsBitPos0b)

    val localDelimNode = state.localDelimiters

    val delimDFAOpt = {
      if (delimiterType == DelimiterTextType.Initiator) localDelimNode.initiator
      else if (delimiterType == DelimiterTextType.Separator) localDelimNode.separator
      else localDelimNode.terminator
    }

    if (!delimDFAOpt.isDefined) Assert.invariantFailed("Expected a delimiter of type " + delimiterType + " on the stack, but was not found.")

    val delimDFA = delimDFAOpt.get

    try {
      val valueString = delimDFA.unparseValue

      val outStream = state.dataOutputStream
      val nCharsWritten = outStream.putString(valueString, state)
      if (nCharsWritten != valueString.length)
        UE(state, "%s - Too many bits in delimiter: IndexOutOfBounds. Insufficient space to write delimiter '%s'.",
          nom, Misc.remapStringToVisibleGlyphs(valueString))
    } catch {
      // Characters in infoset element cannot be encoded without error.
      //
      // This won't actually be thrown until encodingErrorPolicy='error' is
      // implemented.
      //
      case m: MalformedInputException => { UnparseError(One(erd.schemaFileLocation), One(state.currentLocation), "%s - MalformedInputException: \n%s", nom, m.getMessage()) }
      case u: UnmappableCharacterException => { UnparseError(One(erd.schemaFileLocation), One(state.currentLocation), "%s - UnmappableCharacterException: \n%s", nom, u.getMessage()) }
    }
  }

}
