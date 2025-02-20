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

import org.apache.daffodil.runtime1.processors.ModelGroupRuntimeData

/**
 * The purpose of this combinator is to increment/decrement the depth counter
 * for hidden groups, call the group's parser. As we get deeper into the
 * hiddenGroupRefs, we'll increment the counter, call the group's parser, and as
 * we unwind from the refs, we'll decrement.
 */
class HiddenGroupCombinatorParser(ctxt: ModelGroupRuntimeData, bodyParser: Parser)
  extends CombinatorParser(ctxt) {

  override def childProcessors = Vector(bodyParser)

  override def runtimeDependencies = Vector()

  def parse(start: PState): Unit = {
    try {
      start.incrementHiddenDef()
      // parse
      bodyParser.parse1(start)
    } finally {
      start.decrementHiddenDef()
    }
  }
}
