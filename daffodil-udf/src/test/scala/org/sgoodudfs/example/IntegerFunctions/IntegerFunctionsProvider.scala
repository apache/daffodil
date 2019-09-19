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
package org.sgoodudfs.example.IntegerFunctions

import org.apache.daffodil.udf.UserDefinedFunction
import org.apache.daffodil.udf.UserDefinedFunctionProvider
import org.apache.daffodil.udf.UserDefinedFunctionIdentification

/**
 * Example User Defined Function Provider in Scala
 *
 */
class IntegerFunctionsProvider extends UserDefinedFunctionProvider {
  override def getUserDefinedFunctionClasses=
  {
      Array(classOf[BoxedAddition], classOf[PrimitivesAddition])
  }
}

/**
 * Example User Defined Function in Scala
 *
 */
@UserDefinedFunctionIdentification(
    name = "addBoxed",
    namespaceURI = "http://example.com/scala/iudfs")
class BoxedAddition extends UserDefinedFunction {
  /**
   * Adds 2 Boxed Integers
   *
   * @return sum
   */
  def evaluate(num1: Integer, num2: Integer) = {
      val ret = num1 + num2
      ret
  }
}

  /**
   * Example User Defined Function in Scala
   *
   */
  @UserDefinedFunctionIdentification(
      name = "addPrimitive",
      namespaceURI = "http://example.com/scala/iudfs")
  class PrimitivesAddition extends UserDefinedFunction {
  /**
   * Adds 2 primitive Ints
   *
   * @return sum
   */
  def evaluate(num1: Int, num2: Int) = {
      val ret = num1 + num2
      ret
  }
}
