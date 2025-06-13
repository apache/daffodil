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

package org.apache.daffodil.runtime1.processors

import org.apache.daffodil.runtime1.processors.dfa.DFADelimiter

object EmptyDelimiterStackUnparseNode {
  val empty = Array[DFADelimiter]()
  val node = new DelimiterStackUnparseNode(empty, empty, empty)
  def apply() = node
}

object DelimiterStackUnparseNode {

  def apply(
    initiator: Array[DFADelimiter],
    separator: Array[DFADelimiter],
    terminator: Array[DFADelimiter]
  ): DelimiterStackUnparseNode = {
    if (initiator.length == 0 && terminator.length == 0 && separator.length == 0)
      EmptyDelimiterStackUnparseNode()
    else new DelimiterStackUnparseNode(initiator, separator, terminator)
  }

}

class DelimiterStackUnparseNode(
  val initiator: Array[DFADelimiter],
  val separator: Array[DFADelimiter],
  val terminator: Array[DFADelimiter]
) {}
