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

import org.apache.daffodil.exceptions.Assert
import java.lang.reflect.Method
import org.apache.daffodil.udf.UserDefinedFunction

/**
 * Both the evaluate method and the User Defined Function instance are passed in, as both are needed by the Method.invoke
 * function.
 */
case class UserDefinedFunctionCall(recipes: List[CompiledDPath], userDefinedFunction: UserDefinedFunction, evaluateFxn: Method)
  extends FNArgsList(recipes) {
  override def computeValue(values: List[Any], dstate: DState) = {
    val jValues = values.map { _.asInstanceOf[Object] }
    val res = evaluateFxn.invoke(userDefinedFunction, jValues: _*)
    res
  }
}
