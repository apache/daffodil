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

package org.apache.daffodil.runtime1.processors

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.runtime1.iapi.MetadataHandler
import org.apache.daffodil.runtime1.iapi.SequenceMetadata

/**
 * Walks the schema, but not the DSOM schema, it walks the RuntimeData objects that
 * represent the DFDL schema at runtime.
 *
 * @param dp
 */
class MetadataWalker(private val dp: DataProcessor) {

  private lazy val rootERD = dp.ssrd.elementRuntimeData

  def walk(handler: MetadataHandler): Unit = {
    walkTerm(handler, rootERD)
  }

  private def walkTerm(handler: MetadataHandler, trd: TermRuntimeData): Unit = {
    trd match {
      // $COVERAGE-OFF$
      case err: ErrorERD => Assert.invariantFailed("should not get ErrorERDs")
      // $COVERAGE-ON$
      case erd: ElementRuntimeData => walkElement(handler, erd)
      case srd: SequenceRuntimeData => walkSequence(handler, srd)
      case crd: ChoiceRuntimeData => walkChoice(handler, crd)
    }
  }

  private def walkElement(handler: MetadataHandler, erd: ElementRuntimeData): Unit = {
    if (erd.optComplexTypeModelGroupRuntimeData.isDefined)
      walkComplexElement(handler, erd)
    else
      walkSimpleElement(handler, erd)
  }

  private def walkComplexElement(
    handler: MetadataHandler,
    erd: ElementRuntimeData
  ): Unit = {
    val mgrd = erd.optComplexTypeModelGroupRuntimeData.getOrElse {
      // $COVERAGE-OFF$
      Assert.invariantFailed("not a complex type element")
      // $COVERAGE-ON$
    }
    handler.startComplexElementMetadata(erd)
    walkTerm(handler, mgrd)
    handler.endComplexElementMetadata(erd)
  }

  private def walkSimpleElement(
    handler: MetadataHandler,
    erd: ElementRuntimeData
  ): Unit = {
    handler.simpleElementMetadata(erd)
  }

  private def walkSequence(handler: MetadataHandler, sm: SequenceMetadata): Unit = {
    val srd = sm.asInstanceOf[SequenceRuntimeData]
    if (!srd.isHidden) {
      handler.startSequenceMetadata(srd)
      srd.groupMembers.map { trd =>
        walkTerm(handler, trd)
      }
      handler.endSequenceMetadata(srd)
    }
  }

  private def walkChoice(handler: MetadataHandler, crd: ChoiceRuntimeData): Unit = {
    handler.startChoiceMetadata(crd)
    crd.groupMembers.map { trd =>
      walkTerm(handler, trd)
    }
    handler.endChoiceMetadata(crd)
  }

}
