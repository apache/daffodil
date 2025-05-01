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

package org.apache.daffodil.runtime1.processors.parsers

import org.apache.daffodil.runtime1.processors.RuntimeData

trait PStateUsingMacrosMixin {

  /**
   * Creates a new point of uncertainty and binds it to a variable where it can
   * be used. The PoU can be reset, discarded, or resolved, via helper
   * functions. If at the end of the func block, the PoU was not reset,
   * discarded or resolved, it will automatically be discarded. Example usage
   * of this is:
   *
   * pstate.withPointOfUncertainty { pou =>
   * // perform parsing logic that uses the "pou" variable
   * }
   *
   * Note that this is implemented via a macro, and part of this macro magic
   * will munges with variable names in the "func" variable. Thus, this is
   * potentially fragile, e.g. reflection on the pou variable name will fail.
   * It is recommend to keep the func body as small as possible and avoid
   * things like reflection that could cause breakage.
   */
  def withPointOfUncertainty[B](pouID: String, context: RuntimeData)(
    func: PState.Mark => B
  ): B =
    macro PointOfUncertaintyMacros.withPointOfUncertainty[PState.Mark, B]
}
