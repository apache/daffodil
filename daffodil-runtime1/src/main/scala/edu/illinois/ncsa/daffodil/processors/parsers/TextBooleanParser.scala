/* Copyright (c) 2012-2016 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.lang.{ Boolean => JBoolean }
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.TextBooleanTrueRepEv
import edu.illinois.ncsa.daffodil.processors.TextBooleanFalseRepEv

case class ConvertTextBooleanParser(override val context: ElementRuntimeData,
  textBooleanTrueRepEv: TextBooleanTrueRepEv,
  textBooleanFalseRepEv: TextBooleanFalseRepEv,
  ignoreCase: Boolean)
  extends TextPrimParser {

  override lazy val runtimeDependencies = List(textBooleanTrueRepEv, textBooleanFalseRepEv)

  private def matches(str1: String, str2: String): Boolean = {
    if (ignoreCase) str1.equalsIgnoreCase(str2) else str1 == str2
  }

  override def parse(start: PState): Unit = {
    val node = start.simpleElement
    val str = node.dataValueAsString

    Assert.invariant(str != null)

    val textBooleanTrueReps: List[String] = textBooleanTrueRepEv.evaluate(start)
    val textBooleanFalseReps: List[String] = textBooleanFalseRepEv.evaluate(start)

    Assert.invariant(textBooleanTrueReps.length >= 1)
    Assert.invariant(textBooleanFalseReps.length >= 1)

    val newBool: JBoolean =
      if (textBooleanTrueReps.find(matches(_, str)).isDefined) true
      else if (textBooleanFalseReps.find(matches(_, str)).isDefined) false
      else {
        PE(start, "Convert to xs:boolean: Cannot parse boolean from '%s'", str)
        return
      }

    node.overwriteDataValue(newBool)
  }
}
