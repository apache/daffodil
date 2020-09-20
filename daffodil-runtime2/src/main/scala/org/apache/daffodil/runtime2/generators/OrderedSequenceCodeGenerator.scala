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

package org.apache.daffodil.runtime2.generators

import org.apache.daffodil.grammar.primitives.OrderedSequence
import org.apache.daffodil.runtime2.Runtime2CodeGenerator

trait OrderedSequenceCodeGenerator {

  def orderedSequenceGenerateCode(g: OrderedSequence, cgState: CodeGeneratorState): Unit = {
    //
    // To lift this draconian restriction, we have to
    // generate code for each of the children, and combine them into a block
    //
    g.schemaDefinitionUnless(g.sequenceChildren.length == 1, "Only a single child of a sequence is supported.")
    val child1 = g.sequenceChildren(0)
    Runtime2CodeGenerator.generateCode(child1, cgState)
  }
}
