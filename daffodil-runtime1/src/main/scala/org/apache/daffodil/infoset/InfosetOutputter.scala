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

package org.apache.daffodil.infoset


trait InfosetOutputter {

  import Status._

  def status: Status = READY

  /**
   * Reset the internal state of this InfosetOutputter. This should be called
   * inbetween calls to the parse method.
   */
  def reset(): Unit // call to reuse these. When first constructed no reset call is necessary.

  /**
   * Called by Daffodil internals to signify the beginning of the infoset.
   *
   * @return true on sucess, false if there was an error and Daffodil should stop all
   *         future calls to the InfosetOutputter
   */
  def startDocument(): Boolean

  /**
   * Called by Daffodil internals to signify the end of the infoset.
   *
   * @return true on sucess, false if there was an error and Daffodil should stop all
   *         future calls to the InfosetOutputter
   */
  def endDocument(): Boolean

  /**
   * Called by Daffodil internals to signify the beginning of a simple element.
   *
   * @param diSimple the simple element that is started. Various fields of
   *                 DISimple can be accessed to determine things like the
   *                 value, nil, name, namespace, etc.
   * @return true on sucess, false if there was an error and Daffodil should stop all
   *         future calls to the InfosetOutputter
   */

  def startSimple(diSimple: DISimple): Boolean

  /**
   * Called by Daffodil internals to signify the end of a simple element.
   *
   * @param diSimple the simple element that is ended. Various fields of
   *                 DISimple can be accessed to determine things like the
   *                 value, nil, name, namespace, etc.
   * @return true on sucess, false if there was an error and Daffodil should stop all
   *         future calls to the InfosetOutputter
   */
  def endSimple(diSimple: DISimple): Boolean

  /**
   * Called by Daffodil internals to signify the beginning of a complex element.
   *
   * @param diComplex the complex element that is started. Various fields of
   *                  DIComplex can be accessed to determine things like the
   *                  nil, name, namespace, etc.
   * @return true on sucess, false if there was an error and Daffodil should stop all
   *         future calls to the InfosetOutputter
   */
  def startComplex(diComplex: DIComplex): Boolean

  /**
   * Called by Daffodil internals to signify the end of a complex element.
   *
   * @param diComplex the complex element that is ended. Various fields of
   *                  DIComplex can be accessed to determine things like the
   *                  nil, name, namespace, etc.
   * @return true on sucess, false if there was an error and Daffodil should stop all
   *         future calls to the InfosetOutputter
   */
  def endComplex(diComplex: DIComplex): Boolean

  /**
   * Called by Daffodil internals to signify the beginning of an array of elements.
   *
   * @param diComplex the array that is started. Various fields of
   *                  DIArray can be accessed to determine things like the
   *                  name, namespace, etc.
   * @return true on sucess, false if there was an error and Daffodil should stop all
   *         future calls to the InfosetOutputter
   */
  def startArray(diArray: DIArray): Boolean

  /**
   * Called by Daffodil internals to signify the end of an array of elements.
   *
   * @param diComplex the array that is ended. Various fields of
   *                  DIArray can be accessed to determine things like the
   *                  name, namespace, etc.
   * @return true on sucess, false if there was an error and Daffodil should stop all
   *         future calls to the InfosetOutputter
   */
  def endArray(diArray: DIArray): Boolean

  def getStatus(): Status = {
    // Done, Ready (Not started), Visiting (part way done - can retry to visit more)...
    status
  }

  /**
   * Helper function to determine if an element is nilled or not, taking into
   * account whether or not the nilled state has been set yet.
   *
   * @param diElement the element to check the nilled state of
   *
   * @return true if the nilled state has been set and is true. false if the
   *         nilled state is false or if the nilled state has not been set yet
   *         (e.g. during debugging)
   */
  final def isNilled(diElement: DIElement): Boolean = {
    val maybeIsNilled = diElement.maybeIsNilled
    maybeIsNilled.isDefined && maybeIsNilled.get == true
  }
}

object Status extends Enumeration {
  type Status = Value
  val DONE, READY, VISITING = Value
}
