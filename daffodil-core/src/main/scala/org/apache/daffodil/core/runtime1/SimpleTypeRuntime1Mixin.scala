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

import org.apache.daffodil.core.dsom.SimpleTypeDefBase
import org.apache.daffodil.runtime1.processors.SimpleTypeRuntimeData

trait SimpleTypeRuntime1Mixin { self: SimpleTypeDefBase =>

  lazy val simpleTypeRuntimeData: SimpleTypeRuntimeData = {
    val strd =
      new SimpleTypeRuntimeData(
        variableMap,
        schemaFileLocation,
        diagnosticDebugName,
        path,
        namespaces,
        primType,
        noFacetChecks,
        optRestriction.toSeq.flatMap { r => if (r.hasPattern) r.patternValues else Nil },
        optRestriction.flatMap { r => toOpt(r.hasEnumeration, r.enumerationValues.get) },
        optRestriction.flatMap { r => toOpt(r.hasMinLength, r.minLengthValue) },
        optRestriction.flatMap { r => toOpt(r.hasMaxLength, r.maxLengthValue) },
        optRestriction.flatMap { r => toOpt(r.hasMinInclusive, r.minInclusiveValue) },
        optRestriction.flatMap { r => toOpt(r.hasMaxInclusive, r.maxInclusiveValue) },
        optRestriction.flatMap { r => toOpt(r.hasMinExclusive, r.minExclusiveValue) },
        optRestriction.flatMap { r => toOpt(r.hasMaxExclusive, r.maxExclusiveValue) },
        optRestriction.flatMap { r => toOpt(r.hasTotalDigits, r.totalDigitsValue) },
        optRestriction.flatMap { r => toOpt(r.hasFractionDigits, r.fractionDigitsValue) },
        optUnion.orElse(optRestriction.flatMap { _.optUnion }).toSeq.flatMap {
          _.unionMemberTypes.map { _.simpleTypeRuntimeData }
        },
        tunable.unqualifiedPathStepPolicy,
        optRepTypeDef.map(_.simpleTypeRuntimeData),
        optRepValueSet,
        optRepType.map(_.primType),
      )
    strd
  }
  override lazy val runtimeData = simpleTypeRuntimeData
}
