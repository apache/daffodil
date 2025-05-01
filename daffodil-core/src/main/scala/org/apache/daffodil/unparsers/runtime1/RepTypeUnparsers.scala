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

package org.apache.daffodil.unparsers.runtime1

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.runtime1.infoset.DISimple
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueNumber
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueString
import org.apache.daffodil.runtime1.infoset.Infoset
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Success
import org.apache.daffodil.runtime1.processors.unparsers._

class RepTypeUnparser(
  repTypeUnparser: Unparser,
  e: ElementRuntimeData,
  repTypeRuntimeData: ElementRuntimeData,
  logicalValuesMap: Map[DataValueString, DataValueNumber]
) extends CombinatorUnparser(e) {

  override def childProcessors = Vector(repTypeUnparser)

  override def runtimeDependencies = Vector()

  protected def unparse(ustate: UState): Unit = {
    Assert.invariant(ustate.currentInfosetNodeMaybe.isDefined)
    Assert.invariant(ustate.currentInfosetNode.isSimple)

    val currentSimple = ustate.currentInfosetNode.asSimple

    val logicalValueNullable = currentSimple.dataValue
    Assert.invariant(logicalValueNullable.isDefined)
    val logicalValue = logicalValueNullable.getString
    val repValue = logicalValuesMap.getOrElse(
      logicalValue,
      UE(ustate, "Value not found in enumeration: %s", logicalValue)
    )

    val origInfosetElement = ustate.currentInfosetNode
    val tmpInfosetElement = Infoset.newElement(repTypeRuntimeData).asInstanceOf[DISimple]

    // repTypes cannot be used in hidden groups. Elements in a hidden group must either be
    // optional, in which case this parser should never be called, or they require
    // outputValueCalc, which is not allowed to be used in conjunction with dfdlx:repType. If
    // the latter restriction is ever removed, this may require a call to
    // tmpInfosetElement.setHidden()
    Assert.invariant(!ustate.withinHiddenNest)

    // Although quasi elements aren't really part of the infoset, we still
    // require that certain invariants hold. One of which is that all elements
    // have a parent. This is necessary for things like running the
    // InfosetWalker in the interactive debugger. To ensure this invariant
    // holds, we set the parent of this quasi element to the same as that of
    // the current infoset node.
    tmpInfosetElement.setParent(currentSimple.parent)

    if (ustate.processorStatus == Success) {
      tmpInfosetElement.setDataValue(repValue)
      ustate.currentInfosetNodeStack.push(Maybe(tmpInfosetElement))
      repTypeUnparser.unparse1(ustate)
      ustate.currentInfosetNodeStack.pop
    }
  }
}
