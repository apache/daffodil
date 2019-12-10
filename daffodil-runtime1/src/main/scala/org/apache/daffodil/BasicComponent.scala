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

package org.apache.daffodil

import org.apache.daffodil.xml.ResolvesQNames
import org.apache.daffodil.oolag.OOLAG.OOLAGHost
import org.apache.daffodil.processors.HasTunable
import org.apache.daffodil.dsom.ImplementsThrowsOrSavesSDE

/**
 * Base trait shared by all component families in Daffodil
 * including schema components, expression components,
 * grammar subsystem components (e.g, Combinators), etc.
 *
 * Common to them is OOLAG, and availablility of Daffodil's
 * tunables, and the ability to resolve QNames
 */
trait BasicComponent
  extends OOLAGHost
  with HasTunable
  with ResolvesQNames
  with ImplementsThrowsOrSavesSDE {

}
