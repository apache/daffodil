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

package org.apache.daffodil.api

import org.apache.daffodil.util.Enum

  /**
   * Specified how unqualified path steps are resolved.
   *
   * NoNamespace:
   *  Unqualified path steps remain unqualified and will only match elements in
   *  NoNamespace. A prefix must be provided to match namespaced elements.
   *
   * DefaultNamespace:
   *  Unqualified path steps will always use the default namespace. If a default
   *  namespace is defined, it is not possible to match a NoNamespace element
   *  with this policy. Because of this, this may not work well with
   *  elementFormDefault="unqualified".
   *
   * PreferDefaultNamespace
   *  Attempt to use the default namespace to resolve a step. If that fails to
   *  match an element, then try to resolve using NoNamespace.
   */
  object UnqualifiedPathStepPolicy extends Enum {
    abstract sealed trait Type extends EnumValueType
    case object NoNamespace extends Type
    case object DefaultNamespace extends Type
    case object PreferDefaultNamespace extends Type
  }