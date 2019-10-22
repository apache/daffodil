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

import org.apache.daffodil.udf.UserDefinedFunction
import org.apache.daffodil.udf.UserDefinedFunctionService.UserDefinedFunctionMethod
import java.lang.reflect.Method
import java.lang.reflect.InvocationTargetException
import org.apache.daffodil.udf.exceptions.UserDefinedFunctionFatalException
import org.apache.daffodil.udf.exceptions.UserDefinedFunctionProcessingError
import org.apache.daffodil.processors.ProcessingError
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.exceptions.SchemaFileLocation
import org.apache.daffodil.api.DataLocation
import org.apache.daffodil.exceptions.Abort

/**
 * User Defined Function Exception class to wrap processing errors from the UDF
 * into Daffodil Processing Errors
 */
case class UserDefinedFunctionProcessingErrorException(
  errorInfo: String,
  schemaContext: Maybe[SchemaFileLocation],
  dataContext: Maybe[DataLocation],
  errorCause: Maybe[Throwable],
  errorStr: Maybe[String])
  extends ProcessingError(errorInfo, schemaContext, dataContext, errorCause, errorStr)

/**
 * User Defined Function Exception class to wrap fatal errors from the UDF
 * into Daffodil Aborts
 */
case class UserDefinedFunctionFatalErrorException(description: String = "", cause: Throwable, classOfInterest: String = "")
  extends Abort(description ++ cause.getMessage) {

  if (classOfInterest.nonEmpty) {
    val curStackTrace = cause.getStackTrace
    val indexLastUdfEntry = curStackTrace.lastIndexWhere(_.getClassName == classOfInterest)
    //val UdfMentions = curStackTrace.filter(_.getClassName contains "UserDefinedFunction")
    val finalStackTrace = if (indexLastUdfEntry >= 0) curStackTrace.slice(0, indexLastUdfEntry + 1) else curStackTrace
    this.setStackTrace(finalStackTrace) // ++ UdfMentions
  }
}

/**
 * Both the evaluate method and the User Defined Function instance are passed in, as both
 * are needed by the Method.invoke function.
 */
case class UserDefinedFunctionCall(
  functionQNameString: String,
  recipes: List[CompiledDPath],
  userDefinedFunction: UserDefinedFunction,
  evaluateFxn: UserDefinedFunctionMethod)
  extends FNArgsList(recipes) {
  override def computeValue(values: List[Any], dstate: DState) = {
    val jValues = values.map { _.asInstanceOf[Object] }
    try {
      val res = evaluateFxn.method.invoke(userDefinedFunction, jValues: _*)
      res
    } catch {
      case e: InvocationTargetException => {
        // wraps any error thrown by invoked method (i.e UDF.evaluate)
        val targetException = e.getTargetException
        targetException match {
          case te: UserDefinedFunctionProcessingError =>
            throw new UserDefinedFunctionProcessingErrorException(s"User Defined Function '$functionQNameString'",
              Maybe(dstate.compileInfo.schemaFileLocation), dstate.contextLocation, Maybe(te), Maybe.Nope)
          case te: Exception => {
            throw new UserDefinedFunctionFatalErrorException(s"User Defined Function '$functionQNameString' Error: ", te, userDefinedFunction.getClass.getName)
          }
        }
      }
      case e: ReflectiveOperationException =>
        throw new UserDefinedFunctionProcessingErrorException(s"User Defined Function '$functionQNameString'",
          Maybe(dstate.compileInfo.schemaFileLocation), dstate.contextLocation, Maybe(e), Maybe.Nope)
    }
  }
}
