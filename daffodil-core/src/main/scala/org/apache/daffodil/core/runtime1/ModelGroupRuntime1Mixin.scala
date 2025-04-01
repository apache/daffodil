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

package org.apache.daffodil.core.runtime1

import org.apache.daffodil.core.dsom.ElementBase
import org.apache.daffodil.core.dsom.ModelGroup
import org.apache.daffodil.core.dsom.Term
import org.apache.daffodil.runtime1.processors.ModelGroupRuntimeData
import org.apache.daffodil.runtime1.processors.RuntimeData
import org.apache.daffodil.runtime1.processors.TermRuntimeData

trait ModelGroupRuntime1Mixin { self: ModelGroup =>

  requiredEvaluationsIfActivated(modelGroupRuntimeData.initialize)

  final override lazy val runtimeData: RuntimeData = modelGroupRuntimeData

  final override lazy val termRuntimeData: TermRuntimeData = modelGroupRuntimeData

  protected lazy val groupMembersRuntimeData = {
    val res = self.groupMembers.map {
      case eb: ElementBase => eb.erd
      case t: Term => t.termRuntimeData
    }
    res
  }

  def modelGroupRuntimeData: ModelGroupRuntimeData

}
