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

package org.apache.daffodil.runtime1.dpath

// TODO: replace these with our own Infoset implementation objects
import org.apache.daffodil.lib.util.OKOrError
import org.apache.daffodil.runtime1.infoset.DISimple
import org.apache.daffodil.runtime1.processors.parsers.PState

object DFDLCheckConstraintsFunction {

  /**
   * Used for validation purposes when the Validator is DaffodilLimitedValidator.
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
}
