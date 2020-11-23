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

import org.apache.daffodil.dsom.ChoiceBranchImpliedSequence
import org.apache.daffodil.dsom.SequenceTermBase
import org.apache.daffodil.processors.SequenceRuntimeData
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.processors.FillByteUseNotAllowedEv

trait SequenceTermRuntime1Mixin { self: SequenceTermBase =>

  def modelGroupRuntimeData = sequenceRuntimeData

  lazy val sequenceRuntimeData = {
    new SequenceRuntimeData(
      position,
      partialNextElementResolver,
      schemaSet.variableMap,
      encodingInfo,
      // elementChildren.map { _.elementRuntimeData.dpathElementCompileInfo },
      schemaFileLocation,
      dpathCompileInfo,
      diagnosticDebugName,
      path,
      defaultBitOrder,
      groupMembersRuntimeData,
      isRepresented,
      couldHaveText,
      alignmentValueInBits,
      hasNoSkipRegions,
      optIgnoreCase,
      fillByteEv,
      maybeCheckByteAndBitOrderEv,
      maybeCheckBitOrderAndCharsetEv)
  }

}

trait ChoiceBranchImpliedSequenceRuntime1Mixin { self: ChoiceBranchImpliedSequence =>

  override lazy val sequenceRuntimeData: SequenceRuntimeData = {
    new SequenceRuntimeData(
      position,
      partialNextElementResolver,
      schemaSet.variableMap,
      encodingInfo,
      schemaFileLocation,
      dpathCompileInfo,
      diagnosticDebugName,
      path,
      defaultBitOrder,
      groupMembersRuntimeData,
      isRepresented,
      couldHaveText,
      alignmentValueInBits,
      true,
      None,
      FillByteUseNotAllowedEv,
      Maybe.Nope,
      Maybe.Nope)
  }

}
