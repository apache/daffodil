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

package org.apache.daffodil.processors.unparsers

import org.apache.daffodil.processors._
import org.apache.daffodil.schema.annotation.props.gen.TextZonedSignStyle
import org.apache.daffodil.util.DecimalUtils
import org.apache.daffodil.util.DecimalUtils.OverpunchLocation

case class ConvertZonedCombinatorUnparser(
  rd: TermRuntimeData,
  valueUnparser: Unparser,
  converterUnparser: Unparser)
  extends CombinatorUnparser(rd) {

  override lazy val runtimeDependencies = Vector()

  override lazy val childProcessors = Vector(converterUnparser, valueUnparser)

  override def unparse(state: UState): Unit = {
    converterUnparser.unparse1(state)

    if (state.processorStatus eq Success) {
      valueUnparser.unparse1(state)
    }
  }
}

case class ConvertZonedNumberUnparser(
  opl: OverpunchLocation.Value,
  zonedSignStyle: TextZonedSignStyle,
  override val context: ElementRuntimeData)
  extends PrimUnparser
  with ToBriefXMLImpl {

  override lazy val runtimeDependencies = Vector()

  override def unparse(state: UState): Unit = {

    val node = state.currentInfosetNode.asSimple
    val value = node.dataValueAsString

    // The type of value should have the type of S, but type erasure makes this
    // difficult to assert. Could probably check this with TypeTags or Manifest
    // if we find this is not the case. Want something akin to:
    // Assert.invariant(value.isInstanceOf[S])

    val strRep = DecimalUtils.zonedFromNumber(value, zonedSignStyle, opl)

    node.overwriteDataValue(strRep)
  }
}
