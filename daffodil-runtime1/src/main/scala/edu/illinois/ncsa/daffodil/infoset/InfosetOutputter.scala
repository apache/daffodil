/* Copyright (c) 2017 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.infoset


trait InfosetOutputter {

  import Status._

  def status: Status = READY

  def reset(): Unit // call to reuse these. When first constructed no reset call is necessary.

  def startDocument(): Boolean
  def endDocument(): Boolean

  def startSimple(diSimple: DISimple): Boolean // return false to tell caller to stop visiting, true otherwise
  def endSimple(diSimple: DISimple): Boolean

  def startComplex(diComplex: DIComplex): Boolean
  def endComplex(diComplex: DIComplex): Boolean

  def startArray(diArray: DIArray): Boolean
  def endArray(diArray: DIArray): Boolean
  def getStatus(): Status = {
    // Done, Ready (Not started), Visiting (part way done - can retry to visit more)...
    status
  }
}

object Status extends Enumeration {
  type Status = Value
  val DONE, READY, VISITING = Value
}
