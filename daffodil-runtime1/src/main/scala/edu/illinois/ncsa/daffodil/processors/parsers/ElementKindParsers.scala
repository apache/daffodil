/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.api.ValidationMode
import edu.illinois.ncsa.daffodil.processors.Success
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.Parser

class ComplexTypeParser(rd: RuntimeData, bodyParser: Parser)
  extends PrimParser(rd) {
  override def toString = "ComplexType"

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<ComplexType>" +
        bodyParser.toBriefXML(depthLimit - 1) +
        "</ComplexType>"
  }

  def parse(start: PState): PState = {
    start.mpstate.childIndexStack.push(1L) // one-based indexing
    val parseState = bodyParser.parse1(start, rd)
    start.mpstate.childIndexStack.pop()
    parseState
  }
}

class SequenceCombinatorParser(rd: RuntimeData, bodyParser: Parser)
  extends PrimParser(rd) {
  override def toString = "Sequence"

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<Sequence>" +
        bodyParser.toBriefXML(depthLimit - 1) +
        "</Sequence>"
  }

  def parse(start: PState): PState = {
    start.mpstate.groupIndexStack.push(1L) // one-based indexing
    val parseState = bodyParser.parse1(start, rd)
    start.mpstate.groupIndexStack.pop()
    start.mpstate.moveOverOneGroupIndexOnly()
    parseState
  }
}

class ArrayCombinatorParser(erd: ElementRuntimeData, bodyParser: Parser) extends PrimParser(erd) {
  override def toString = "Array"

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<Array>" +
        bodyParser.toBriefXML(depthLimit - 1) +
        "</Array>"
  }

  def parse(start: PState): PState = {

    start.mpstate.arrayIndexStack.push(1L) // one-based indexing
    start.mpstate.occursBoundsStack.push(DaffodilTunableParameters.maxOccursBounds)

    val parseState = bodyParser.parse1(start, erd)
    if (parseState.status != Success) return parseState

    val shouldValidate = start.mpstate.dataProc.getValidationMode != ValidationMode.Off

    val actualOccurs = start.mpstate.arrayIndexStack.pop()
    start.mpstate.occursBoundsStack.pop()

    val finalState = {
      (erd.minOccurs, erd.maxOccurs) match {
        case (Some(minOccurs), Some(maxOccurs)) if shouldValidate =>
          val isUnbounded = maxOccurs == -1
          val occurrence = actualOccurs - 1
          val postValidationState = {
            if (isUnbounded && occurrence < minOccurs)
              parseState.withValidationError("%s occurred '%s' times when it was expected to be a " +
                "minimum of '%s' and a maximum of 'UNBOUNDED' times.", erd.prettyName,
                occurrence, minOccurs)
            else if (!isUnbounded && (occurrence < minOccurs || occurrence > maxOccurs))
              parseState.withValidationError("%s occurred '%s' times when it was expected to be a " +
                "minimum of '%s' and a maximum of '%s' times.", erd.prettyName,
                occurrence, minOccurs, maxOccurs)
            else
              parseState
          }
          postValidationState
        case _ => parseState
      }
    }
    finalState
  }
}
