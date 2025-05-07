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

package org.apache.daffodil.runtime1.udf

import org.apache.daffodil.lib.exceptions.Abort
import org.apache.daffodil.lib.exceptions.SchemaFileLocation
import org.apache.daffodil.lib.iapi.DataLocation
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.runtime1.processors.ProcessingError

/**
 * User Defined Function Exception class to wrap processing errors from the UDF
 * into Daffodil Processing Errors
 */
case class UserDefinedFunctionProcessingErrorException(
  errorInfo: String,
  schemaContext: Maybe[SchemaFileLocation],
  dataContext: Maybe[DataLocation],
  errorCause: Maybe[Throwable],
  errorStr: Maybe[String]
) extends ProcessingError(errorInfo, schemaContext, dataContext, errorCause, errorStr)

/**
 * User Defined Function Exception class to wrap fatal errors from the UDF
 * into Daffodil Aborts
 */
case class UserDefinedFunctionFatalErrorException(
  description: String = "",
  cause: Throwable,
  udfOfInterest: String = "",
  providerOfInterest: String = ""
) extends Abort(description + ". Cause: " + cause.toString) {

  /*
   * This will replace the stacktrace of the fatal error with just the UDF relevant
   * portions; removing the Daffodil (incl our evaluate invocation) and the Method
   * reflection portions.
   *
   */
  val classesOfInterest = List(udfOfInterest, providerOfInterest).filterNot(Misc.isNullOrBlank)
  if (classesOfInterest.nonEmpty) {
    val curStackTrace = cause.getStackTrace
    val indexLastUdfEntry =
      curStackTrace.lastIndexWhere(ste => classesOfInterest.exists(_ == ste.getClassName))
    val finalStackTrace =
      if (indexLastUdfEntry >= 0) curStackTrace.slice(0, indexLastUdfEntry + 1)
      else curStackTrace
    this.setStackTrace(finalStackTrace)
  }
}
