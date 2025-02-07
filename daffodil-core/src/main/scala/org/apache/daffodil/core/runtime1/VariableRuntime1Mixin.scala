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

import org.apache.daffodil.core.dsom.DFDLDefineVariable
import org.apache.daffodil.core.dsom.DFDLNewVariableInstance
import org.apache.daffodil.core.dsom.DFDLSetVariable
import org.apache.daffodil.core.dsom.ExpressionCompilers
import org.apache.daffodil.core.dsom.VariableReference
import org.apache.daffodil.lib.schema.annotation.props.Found
import org.apache.daffodil.lib.util.Delay
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.xml.GlobalQName
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.processors.VariableRuntimeData

trait DFDLDefineVariableRuntime1Mixin { self: DFDLDefineVariable =>

  requiredEvaluationsAlways(variableRuntimeData.initialize)

  final lazy val variableRuntimeData = {
    val index = this.schemaSet.allDefinedVariables.indexOf(this)
    val vrd = new VariableRuntimeData(
      this.schemaFileLocation,
      this.diagnosticDebugName,
      this.path,
      this.namespaces,
      this.noPrefixNamespace,
      this.external,
      this.direction,
      Delay(Symbol("maybeDefaultValueExpr"), this, maybeDefaultValueExpr),
      this.typeQName,
      this.namedQName.asInstanceOf[GlobalQName],
      this.primType,
      this.tunable.unqualifiedPathStepPolicy,
      vmapIndex = index
    )
    vrd
  }

  final override lazy val runtimeData = variableRuntimeData

  lazy val maybeDefaultValueExpr = {
    val compilationTargetType = primType
    val qn = this.qNameForProperty("defaultValue", XMLUtils.dafintURI)
    val defaultValExpr = defaultValue.map { e =>
      ExpressionCompilers.AnyRef.compileProperty(
        qn,
        compilationTargetType,
        Found(e, this.dpathCompileInfo, "defaultValue", false),
        this,
        dpathCompileInfo
      )
    }

    Maybe.toMaybe(defaultValExpr)
  }
}

trait VariableReferenceRuntime1Mixin { self: VariableReference =>

  def variableRuntimeData: VariableRuntimeData

}

trait DFDLNewVariableInstanceRuntime1Mixin { self: DFDLNewVariableInstance =>

  requiredEvaluationsIfActivated(variableRuntimeData.initialize)

  /* Need to override variableRuntimeData so that defaultValues
   * are read from newVariableInstance instead of the original
   * variable definition. Also allows diagnostic messages to
   * point to this location instead of the original definition
   */
  final override lazy val variableRuntimeData = {
    val globalVRD = defv.variableRuntimeData
    val vrd = new VariableRuntimeData(
      this.schemaFileLocation,
      this.diagnosticDebugName,
      this.path,
      this.namespaces,
      this.noPrefixNamespace,
      defv.external,
      defv.direction,
      Delay(Symbol("maybeDefaultValueExpr2"), this, maybeDefaultValueExpr),
      defv.typeQName,
      globalVRD.globalQName, // important that this is the exact same globalQName object as is used by the globalVRD.
      defv.primType,
      this.tunable.unqualifiedPathStepPolicy,
      // This is a really important invariant. The index of the NVI's VRD
      // must be identical to that of the global VRD for this variable,
      // So we take it from there.
      vmapIndex = globalVRD.vmapIndex
    )
    vrd
  }

  lazy val maybeDefaultValueExpr = {
    val compilationTargetType = defv.primType
    val qn = this.qNameForProperty("defaultValue", XMLUtils.dafintURI)
    val defaultValExpr = defaultValue.map { e =>
      ExpressionCompilers.AnyRef.compileProperty(
        qn,
        compilationTargetType,
        Found(e, this.dpathCompileInfo, "defaultValue", false),
        this,
        dpathCompileInfo
      )
    }

    Maybe.toMaybe(defaultValExpr)
  }
}

trait DFDLSetVariableRuntime1Mixin { self: DFDLSetVariable =>

  final override def variableRuntimeData = defv.variableRuntimeData
}
