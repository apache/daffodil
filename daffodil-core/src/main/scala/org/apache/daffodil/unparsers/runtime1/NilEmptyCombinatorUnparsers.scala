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
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.unparsers._

case class SimpleNilOrValueUnparser(
  ctxt: ElementRuntimeData,
  nilUnparser: Unparser,
  valueUnparser: Unparser
) extends CombinatorUnparser(ctxt) {

  override def runtimeDependencies = Vector()

  override def childProcessors = Vector(nilUnparser, valueUnparser)

  def unparse(state: UState): Unit = {
    Assert.invariant(Maybe.WithNulls.isDefined(state.currentInfosetNode))
    val inode = state.currentInfosetNode.asSimple
    // If this element has dfdl:outputValueCalc defined then the nilled value
    // is always ignored and only the OVC value is used. Furthermore, if the
    // element does not have xsi:nilled="true" in the infoset, that means
    // _isNilledSet will be false until the OVC is evaluated. Thus, if the OVC
    // suspends, this call to isNilled will throw a InfosetNoDataException
    // because _isNilled has not been set yet. Rather than having to deal with
    // suspending, only check isNilled for non-OVC elements.
    if (!ctxt.dpathElementCompileInfo.isOutputValueCalc && inode.isNilled)
      nilUnparser.unparse1(state)
    else valueUnparser.unparse1(state)
  }
}

case class ComplexNilOrContentUnparser(
  ctxt: ElementRuntimeData,
  nilUnparser: Unparser,
  contentUnparser: Unparser
) extends CombinatorUnparser(ctxt) {

  override def runtimeDependencies = Vector()

  override def childProcessors = Vector(nilUnparser, contentUnparser)

  def unparse(state: UState): Unit = {
    Assert.invariant(Maybe.WithNulls.isDefined(state.currentInfosetNode))
    val inode = state.currentInfosetNode.asComplex
    if (inode.isNilled)
      nilUnparser.unparse1(state)
    else
      contentUnparser.unparse1(state)
  }
}
