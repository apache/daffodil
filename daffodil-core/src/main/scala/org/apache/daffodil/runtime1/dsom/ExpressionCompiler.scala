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

package org.apache.daffodil.runtime1.dsom

import java.lang.{ Boolean => JBoolean, Long => JLong }
import scala.xml.NamespaceBinding

import org.apache.daffodil.lib.xml.NS
import org.apache.daffodil.lib.xml.NamedQName
import org.apache.daffodil.runtime1.BasicComponent
import org.apache.daffodil.runtime1.dpath.NodeInfo

trait ExpressionCompilerBase[T <: AnyRef] {

  def compileExpression(
    qn: NamedQName,
    nodeInfoKind: NodeInfo.Kind,
    exprWithBracesMaybe: String,
    namespaces: NamespaceBinding,
    noPrefixNamespace: NS,
    compileInfoWherePropertyWasLocated: DPathCompileInfo,
    isEvaluatedAbove: Boolean,
    host: BasicComponent,
    compileInfo: DPathCompileInfo
  ): CompiledExpression[T]
}

abstract class ExpressionCompilerClass {
  def String: ExpressionCompilerBase[String]
  def JLong: ExpressionCompilerBase[JLong]
  def AnyRef: ExpressionCompilerBase[AnyRef]
  def JBoolean: ExpressionCompilerBase[JBoolean]
}
