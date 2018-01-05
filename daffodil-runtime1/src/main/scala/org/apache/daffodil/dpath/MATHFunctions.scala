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

package org.apache.daffodil.dpath

import org.apache.daffodil.util.Numbers._
import java.lang.{ Double => JDouble }

case class MATHPow(recipes: List[CompiledDPath]) extends FNTwoArgs(recipes) {
  override def computeValue(arg1: AnyRef, arg2: AnyRef, dstate: DState): AnyRef = {
    val base = asDouble(arg1).doubleValue()
    val exp = asDouble(arg2).doubleValue()
    if (exp.isInfinite && (base == 1 || base == -1)) {
      // java pow(+-1, +-inf) returns NaN, XPath says it should be 1.0
      new JDouble(1.0)
    } else if (exp.isNaN && base == 1) {
      // java pow(1, NaN) returns NaN, XPath says it should be 1.0
      new JDouble(1.0)  
    } else {
      new JDouble(java.lang.Math.pow(base, exp))
    }
  }
}
