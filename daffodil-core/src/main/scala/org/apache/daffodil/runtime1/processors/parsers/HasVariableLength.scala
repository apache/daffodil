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

import java.lang.{ Long => JLong }

import org.apache.daffodil.lib.util.Numbers
import org.apache.daffodil.runtime1.processors.Evaluatable

trait HasVariableLength { self: PrimParser =>
  def lengthEv: Evaluatable[JLong]

  def getLength(pstate: PState): Long = {
    val lengthAsJLong = lengthEv.evaluate(pstate)
    val l = Numbers.asLong(lengthAsJLong)
    l
  }
}

trait HasFixedLength { self: PrimParser =>

  def nItems: Long
  def getLength(pstate: PState) = nItems
}
