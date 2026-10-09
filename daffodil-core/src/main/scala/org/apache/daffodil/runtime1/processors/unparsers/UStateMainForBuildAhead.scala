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

package org.apache.daffodil.runtime1.processors.unparsers

import org.apache.daffodil.lib.iapi.DaffodilTunables
import org.apache.daffodil.lib.iapi as api
import org.apache.daffodil.runtime1.infoset.DINode
import org.apache.daffodil.runtime1.infoset.InfosetInputter
import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.runtime1.processors.VariableMap

/**
 * The UState for unparsing a tree that build made ahead of it, by reading
 * the tree as events and taking each node from the tree.
 */
final class UStateMainForBuildAhead private[unparsers] (
  inputter: InfosetInputter,
  outStream: java.io.OutputStream,
  vmap: VariableMap,
  diagnosticsArg: Seq[api.Diagnostic],
  dataProcArg: DataProcessor,
  tunable: DaffodilTunables,
  areDebugging: Boolean,
  override protected val treeEvents: TreeEventState
) extends UStateMain(
    inputter,
    outStream,
    vmap,
    diagnosticsArg,
    dataProcArg,
    tunable,
    areDebugging,
    treeEvents
  )
  with InfosetFromTree {

  // How far the unparse of the built tree has reached, for the debugger.
  def reachedChildCounts(): java.util.Map[DINode, Integer] = treeEvents.reachedChildCounts()
}
