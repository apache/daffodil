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

import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.processors.SequenceRuntimeData
import org.apache.daffodil.exceptions.Assert

abstract class OrderedSequenceUnparserBase(
  srd:            SequenceRuntimeData,
  childUnparsers: Vector[Unparser])
  extends CombinatorUnparser(srd) {

  override def nom = "Sequence"

  override lazy val runtimeDependencies: Vector[Evaluatable[AnyRef]] = Vector()

  override lazy val childProcessors = childUnparsers

  // Sequences of nothing (no initiator, no terminator, nothing at all) should
  // have been optimized away
  Assert.invariant(childUnparsers.length > 0)

  // Since some of the grammar terms might have folded away to EmptyGram,
  // the number of unparsers here may be different from the number of
  // children of the sequence group.
  Assert.invariant(srd.groupMembers.length >= childUnparsers.length - 1) // minus 1 for the separator unparser
}
