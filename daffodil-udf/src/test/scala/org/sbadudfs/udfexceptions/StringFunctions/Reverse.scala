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
package org.sbadudfs.udfexceptions.StringFunctions

import org.apache.daffodil.udf.UserDefinedFunction
import org.apache.daffodil.udf.UserDefinedFunctionIdentification
import com.sun.xml.internal.bind.v2.runtime.IllegalAnnotationsException

/**
 * UDF for exceptions negative tests in Scala
 *
 * Throws exception on evaluate
 */
@UserDefinedFunctionIdentification(name = "reverse",
  namespaceURI = "http://scala.udf.com")
class Reverse extends UserDefinedFunction {
  final case class CustomException(
    private val message: String = "",
    private val cause: Throwable = None.orNull)
    extends Exception(message, cause)

  /**
   * Reverses the order of chars in a string
   *
   * @param str string whose order you wish to reverse
   * @return reversed str
   */
  def evaluate(str: String) = {
    val ret = str.reverse
    throw new CustomException("UDF Error!")
    ret
  }
}