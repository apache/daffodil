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

package org.apache.daffodil.runtime1

import org.apache.daffodil.lib.oolag.OOLAG.OOLAGHost
import org.apache.daffodil.runtime1.dsom.ImplementsThrowsOrSavesSDE
import org.apache.daffodil.runtime1.processors.HasTunable

/**
 * Base trait shared by all component families in Daffodil
 * including schema components, expression components,
 * grammar subsystem components (e.g, Combinators), etc.
 *
 * Common to them is OOLAG, and availablility of Daffodil's
 * tunables.
 */
trait BasicComponent extends OOLAGHost with HasTunable with ImplementsThrowsOrSavesSDE {

  /**
   * Components have an initialize protocol.
   *
   * Use object factories (private constructor on the class) to
   * enforce calling of initialize() after construction.
   *
   * The overrides of initialize should always call super.initialize()
   *
   * They should evaluate any lazy vals that MUST be evaluatable upon
   * object construction - typically this is things that may be needed
   * in order to issue diagnostic messages.
   */
  protected def initialize(): Unit = {
    // do nothing.
  }

}
