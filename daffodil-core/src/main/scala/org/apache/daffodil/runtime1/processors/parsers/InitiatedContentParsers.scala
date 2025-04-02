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

package org.apache.daffodil.runtime1.processors.parsers

import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.TermRuntimeData

final class InitiatedContentDiscrimOnIndexGreaterThanMinParser(
  min: Int,
  override val context: ElementRuntimeData
) extends PrimParser {
  override def runtimeDependencies = Vector()

  final def parse(start: PState): Unit = {
    if (start.arrayIterationPos > min)
      start.resolvePointOfUncertainty()
  }
}

final class InitiatedContentDiscrimChoiceParser(override val context: TermRuntimeData)
  extends PrimParser {
  override def runtimeDependencies = Vector()

  final def parse(start: PState): Unit = {
    start.resolvePointOfUncertainty()
  }
}

final class InitiatedContentDiscrimChoiceOnlyOnFirstIndexParser(
  override val context: TermRuntimeData
) extends PrimParser {
  override def runtimeDependencies = Vector()

  final def parse(start: PState): Unit = {
    if (start.arrayIterationPos == 1)
      start.resolvePointOfUncertainty()
  }
}

final class InitiatedContentDiscrimChoiceAndIndexGreaterThanMinParser(
  min: Int,
  override val context: ElementRuntimeData
) extends PrimParser {
  override def runtimeDependencies = Vector()

  final def parse(start: PState): Unit = {
    // Resolves PoUs associated with arrays with some minimum number of
    // required occurrences with initiated content.
    //
    // Note that if min is zero (i.e the array is optional), this does mean
    // that two points of uncertainty will be resolved. This may seem wrong,
    // but it is actually the correct behavior. We first resolve the PoU
    // associated with the optional array, thus saying that the first element
    // of the optional array does exist. We then resolve the second PoU for the
    // initiated content choice, which says that the choice picked the correct
    // branch and not to try any other branches if something in the array
    // fails.

    if (start.arrayIterationPos > min) {
      // resolve the point of uncertainty associated with array elements once
      // we have parsed the required number of min occurrences. There should
      // not be a point of uncertainty associated with this array until we have
      // parsed more than min occurrences
      start.resolvePointOfUncertainty()
    }

    if (start.arrayIterationPos == 1) {
      // resolve the point of uncertainty associated with the initiated content
      // choice so we do not attempt to parse other branches if something fails
      start.resolvePointOfUncertainty()
    }
  }
}
