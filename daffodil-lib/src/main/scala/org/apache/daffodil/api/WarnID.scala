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

import org.apache.daffodil.exceptions.ThrowsSDE
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.schema.annotation.props.{ Enum => PropsEnum }

  sealed trait WarnID extends WarnID.Value

  object WarnID extends PropsEnum[WarnID] {
    case object All extends WarnID; forceConstruction(All)
    case object MultipleChoiceBranches extends WarnID; forceConstruction(MultipleChoiceBranches)
    case object EscapeSchemeRefUndefined extends WarnID; forceConstruction(EscapeSchemeRefUndefined)

    def apply(name: String, context: ThrowsSDE) = Assert.usageError("not to be called. Call find(name) method instead.")

    def find(name: String): Option[WarnID] = optionStringToEnum("warning identifier", name)
  }
