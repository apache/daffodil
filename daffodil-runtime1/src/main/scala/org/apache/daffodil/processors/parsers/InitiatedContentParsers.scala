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

package org.apache.daffodil.processors.parsers

import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.TermRuntimeData

final class InitiatedContentDiscrimOnIndexGreaterThanMinParser(
  min: Int,
  override val context: ElementRuntimeData)
  extends PrimParser {
  override lazy val runtimeDependencies = Vector()

  def parse(start: PState): Unit = {
    if (start.arrayPos > min)
      start.setDiscriminator(true)
  }
}

final class InitiatedContentDiscrimChoiceParser(override val context: TermRuntimeData)
  extends PrimParser {
  override lazy val runtimeDependencies = Vector()

  final def parse(start: PState): Unit = {
    start.setDiscriminator(true)
  }
}

final class InitiatedContentDiscrimChoiceAndIndexGreaterThanMinParser(
  min: Int,
  override val context: ElementRuntimeData)
  extends PrimParser {
  override lazy val runtimeDependencies = Vector()

  def parse(start: PState): Unit = {
    val top = start.discriminator
    start.popPointOfUncertainty // pop array discriminator off of stack and any changed variables
    start.setDiscriminator(true) // set the choice discriminator
    start.pushPointOfUncertainty
    start.setDiscriminator(top) // restore the array discriminator to top of stack
    if (start.arrayPos > min)
      start.setDiscriminator(true) // set array discriminator only if
  }
}
