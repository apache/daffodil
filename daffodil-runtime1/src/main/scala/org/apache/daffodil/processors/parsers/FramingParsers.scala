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

import org.apache.daffodil.processors.TextProcessor
import org.apache.daffodil.processors.TermRuntimeData

class SkipRegionParser(
  skipInBits: Int,
  override val context: TermRuntimeData)
  extends PrimParser {

  override lazy val runtimeDependencies = Vector()

  override def parse(pstate: PState) = {
    val dis = pstate.dataInputStream
    if (!dis.skip(skipInBits, pstate)) PE(pstate, "Unable to skip %s bits.", skipInBits)
  }
}

class AlignmentFillParser(
  alignmentInBits: Int,
  override val context: TermRuntimeData)
  extends PrimParser {

  override lazy val runtimeDependencies = Vector()

  override def parse(pstate: PState): Unit = {
    val dis = pstate.dataInputStream
    if (!dis.align(alignmentInBits, pstate))
      PE(pstate, "Unable to align to %s(bits)", alignmentInBits)
  }
}

class MandatoryTextAlignmentParser(
  alignmentInBits: Int,
  override val context: TermRuntimeData)
  extends AlignmentFillParser(alignmentInBits, context)
  with TextProcessor
