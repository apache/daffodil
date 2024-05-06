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

package org.apache.daffodil.core.dsom

import org.apache.daffodil.lib.schema.annotation.props.PropertyLookupResult
import org.apache.daffodil.runtime1.dsom._

trait PropertyReferencedElementInfosMixin {
  protected final type F =
    ContentValueReferencedElementInfoMixin => Set[DPathElementCompileInfo]

  /**
   * Convenience method to make gathering up all elements referenced in expressions
   * easier.
   */
  protected final def propExprElts(
    rawProp: PropertyLookupResult,
    evArg: => ContentValueReferencedElementInfoMixin,
    f: F
  ): Set[DPathElementCompileInfo] = {
    lazy val ev = evArg
    if (rawProp.isDefined) f(ev) else Set()
  }

  protected def propertyContentReferencedElementInfos: Set[DPathElementCompileInfo]
  protected def propertyValueReferencedElementInfos: Set[DPathElementCompileInfo]

  protected def statementContentParserReferencedElementInfos: Set[DPathElementCompileInfo]
  protected def calcContentParserReferencedElementInfos = ReferencedElementInfos.None

  protected def statementContentUnparserReferencedElementInfos: Set[DPathElementCompileInfo]
  protected def calcContentUnparserReferencedElementInfos = ReferencedElementInfos.None

  protected def statementValueParserReferencedElementInfos: Set[DPathElementCompileInfo]
  protected def calcValueParserReferencedElementInfos = ReferencedElementInfos.None

  protected def statementValueUnparserReferencedElementInfos: Set[DPathElementCompileInfo]
  protected def calcValueUnparserReferencedElementInfos = ReferencedElementInfos.None

}
