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

package org.apache.daffodil.runtime1.dpath

import java.lang.reflect.InvocationTargetException

import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.runtime1.infoset.DataValue
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.udf.UserDefinedFunctionFatalErrorException
import org.apache.daffodil.runtime1.udf.UserDefinedFunctionProcessingErrorException
import org.apache.daffodil.runtime1.udf.UserDefinedFunctionService.UserDefinedFunctionMethod
import org.apache.daffodil.udf.UserDefinedFunction
import org.apache.daffodil.udf.exceptions.UserDefinedFunctionProcessingError

/**
 * Both the serializable evaluate method and the User Defined Function instance are passed in,
 * as both are needed by the Method.invoke function.
 */
case class UserDefinedFunctionCall(
  functionQNameString: String,
  recipes: List[CompiledDPath],
  userDefinedFunction: UserDefinedFunction,
  evaluateFxn: UserDefinedFunctionMethod
) extends FNArgsList(recipes) {

  override def computeValue(values: List[DataValuePrimitive], dstate: DState) = {
    val jValues = values.map { _.getAnyRef.asInstanceOf[Object] }
    try {
      val res = evaluateFxn.method.invoke(userDefinedFunction, jValues: _*)
      DataValue.unsafeFromAnyRef(res)
    } catch {
      case e: InvocationTargetException => {
        // wraps any error thrown by invoked method (i.e UDF.evaluate)
        val targetException = e.getTargetException
        targetException match {
          case te: UserDefinedFunctionProcessingError =>
            throw new UserDefinedFunctionProcessingErrorException(
              s"User Defined Function '$functionQNameString'",
              Maybe(dstate.compileInfo.schemaFileLocation),
              dstate.contextLocation,
              Maybe(te),
              Maybe.Nope
            )
          case te: Exception =>
            throw new UserDefinedFunctionFatalErrorException(
              s"User Defined Function '$functionQNameString' Error",
              te,
              userDefinedFunction.getClass.getName
            )
        }
      }
      case e @ (_: IllegalArgumentException | _: NullPointerException |
          _: ReflectiveOperationException) =>
        throw new UserDefinedFunctionProcessingErrorException(
          s"User Defined Function '$functionQNameString'",
          Maybe(dstate.compileInfo.schemaFileLocation),
          dstate.contextLocation,
          Maybe(e),
          Maybe.Nope
        )
      case e: ExceptionInInitializerError =>
        throw new UserDefinedFunctionFatalErrorException(
          s"User Defined Function '$functionQNameString' Error",
          e,
          userDefinedFunction.getClass.getName
        )
    }
  }
}
