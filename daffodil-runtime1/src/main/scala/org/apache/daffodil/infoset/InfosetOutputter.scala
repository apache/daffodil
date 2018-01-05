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
