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

package edu.illinois.ncsa.daffodil.dpath

// TODO: replace these with our own Infoset implementation objects

import edu.illinois.ncsa.daffodil.processors.parsers.PState
import edu.illinois.ncsa.daffodil.infoset.DISimple
import edu.illinois.ncsa.daffodil.util.OKOrError

object DFDLCheckConstraintsFunction {
  /**
   * Used for validation purposes when ValidationMode is Limited or Full.
   *
   * Provides the result Unit on Success or a message (String) on Failure.
   *
   * @param pstate the state containing the currentElement, data, primType and context.
   *
   * @return a Boolean on success, String (message) on failure.
   */
  def validate(pstate: PState): OKOrError = {
    executeCheck(pstate.infoset.asInstanceOf[DISimple])
  }

  /**
   * Performs the constraint checks using information contained within the
   * PState object.
   *
   * @param pstate the current parser state.
   *
   * @return a Unit on success, String (message) on failure.
   */

  def executeCheck(currentElement: DISimple): OKOrError = {
    val optSTRD = currentElement.erd.optSimpleTypeRuntimeData
    if (optSTRD.isEmpty) OKOrError.OK
    else {
      val e = optSTRD.get
      val res = e.executeCheck(currentElement)
      res
    }
  }

  //  def checkOccurrence(minOccurs: Int, maxOccurs: Int, position: Long): Boolean = {
  //    // A maxOccurs of -1 signifies unbounded
  //    if ( // position > minOccurs && // DON"T CHECK MIN OCCURS.
  //    // That can't work. If minOccurs is 5 the first element at position 1 will fail this check.
  //    ((position <= maxOccurs) || (maxOccurs == -1))) { return true }
  //    return false
  //  }
}
