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

package org.apache.daffodil.udf

import collection.JavaConverters._
import collection.mutable._;
import java.util.ServiceLoader
import java.util.ServiceConfigurationError
import org.apache.daffodil.util.Misc
import org.apache.daffodil.util.LogLevel
import org.apache.daffodil.util.Logging
import java.lang.reflect.Method
import org.apache.daffodil.dpath.NodeInfo
import java.io.ObjectOutputStream
import java.io.Serializable
import java.io.ObjectInputStream

/**
 * Loads, validates and caches (for use at schema compile time) all User Defined Functions
 * and their Providers from the classpath.
 *
 */
object UserDefinedFunctionService extends Logging {
  lazy val evaluateMethodName = "evaluate"

  case class UserDefinedFunctionMethod(
      decClass: Class[_],
      methodName: String,
      paramTypes: Array[Class[_]])
      extends Serializable {
    var method: Method = _

    def getMethod = this.method

    def writeObject(out: ObjectOutputStream) {
      out.writeObject(decClass)
      out.writeUTF(methodName)
      out.writeObject(paramTypes)
    }

    def readObject(in: ObjectInputStream) {
      val decClass: Class[_] = in.readObject().asInstanceOf[Class[_]]
      val methodName = in.readUTF()
      val paramTypes = in.readObject().asInstanceOf[Array[Class[_]]]

      this.method = decClass.getMethod(methodName, paramTypes: _*)
    }
  }
  case class EvaluateMethodInfo(evaluateMethod: UserDefinedFunctionMethod, parameterTypes: List[NodeInfo.Kind], returnType: NodeInfo.Kind)
  case class UserDefinedFunctionCallingInfo(udf: UserDefinedFunction, evalInfo: EvaluateMethodInfo)
  case class UserDefinedFunctionInfo(udfClass: Class[_], provider: UserDefinedFunctionProvider, evaluateMethodInfo: EvaluateMethodInfo)
  type NamespaceUriAndName = String

  private val udfInfoLookup: HashMap[NamespaceUriAndName, UserDefinedFunctionInfo] = HashMap()
  private val initializedUserDefinedFunctionsCache: HashMap[NamespaceUriAndName, Option[UserDefinedFunctionCallingInfo]] = HashMap()

  lazy val classUserDefinedFunctionIdentification = classOf[UserDefinedFunctionIdentification]
  lazy val classUserDefinedFunction = classOf[UserDefinedFunction]
  lazy val currentClassPath = Misc.classPath.map(_.toString).mkString("\n")

  val loader: ServiceLoader[UserDefinedFunctionProvider] = ServiceLoader.load(classOf[UserDefinedFunctionProvider])

  try {
    loader.asScala.foreach { provider =>
      lazy val providerClassName = provider.getClass.getName
      val providerFunctionClasses =
        /*
         * This is to protect against any errors thrown when we are trying to load the
         * UDFs. It will catch any exceptions and emit them as the reason the UDF was
         * dropped
         */
        try {
          provider.getUserDefinedFunctionClasses
        } catch {
          case e: Exception =>
            log(LogLevel.Warning,
              "User Defined Function Provider ignored: %s. Error loading User Defined Functions: %s",
              providerClassName, e)
            null
        }

      if (providerFunctionClasses == null || providerFunctionClasses.isEmpty) {
        log(LogLevel.Warning, "User Defined Function Provider ignored: %s. No User Defined Functions found.",
          providerClassName)
      } else {

        lazy val goodFunctionClasses = providerFunctionClasses.filter {
          udfc =>
            val nonAnn = if (!udfc.isAnnotationPresent(classUserDefinedFunctionIdentification)) {
              log(LogLevel.Warning, "User Defined Function ignored: %s. Missing %s annotation",
                udfc.getName, classUserDefinedFunctionIdentification.getName)
              true
            } else false
            val nonUdf = if (!classUserDefinedFunction.isAssignableFrom(udfc)) {
              log(LogLevel.Warning, "User Defined Function ignored: %s. Doesn't implement %s",
                udfc.getName, classUserDefinedFunction.getName)
              true
            } else false
            if (nonAnn || nonUdf) false
            else true
        }

        goodFunctionClasses.foreach { fc =>
          val fcClassName = fc.getName
          val fInfo = fc.getDeclaredAnnotation(classUserDefinedFunctionIdentification)
          val fns = fInfo.namespaceURI()
          val fname = fInfo.name()
          val invalidFns = Misc.isNullOrBlank(fns)
          val invalidFname = Misc.isNullOrBlank(fname)
          val evalMethods = fc.getMethods.filter(_.getName == evaluateMethodName)

          if (invalidFns || invalidFname) {
            if (invalidFns)
              log(LogLevel.Warning,
                "User Defined Function ignored: %s. Annotation namespace field is empty or invalid.",
                fcClassName)
            if (invalidFname)
              log(LogLevel.Warning,
                "User Defined Function ignored: %s. Annotation name field is empty or invalid.",
                fcClassName)
          } else if (evalMethods.isEmpty) {
            log(LogLevel.Warning, "User Defined Function ignored: %s. Missing evaluate method: %s:%s",
              fcClassName, fns, fname)
          } else if (evalMethods.length > 1) {
            log(LogLevel.Warning, "User Defined Function ignored: %s. Overloaded evaluate method: %s:%s",
              fcClassName, fns, fname)
          } else {
            val evaluateMethod = evalMethods.head
            val paramTypes = evaluateMethod.getParameterTypes
            val returnType = evaluateMethod.getReturnType

            val initParamTypeConv = paramTypes.map { c => NodeInfo.fromClass(c) }
            val initRetTypeConv = NodeInfo.fromClass(returnType)
            val badParamIndex = initParamTypeConv.indexOf(None)
            val invalidParamType = badParamIndex >= 0
            val invalidRetType = initRetTypeConv.isEmpty

            if (invalidParamType || invalidRetType) {
              if (invalidParamType) {
                log(LogLevel.Warning, "User Defined Function ignored: %s. Unsupported parameter type: %s",
                  fcClassName, paramTypes(badParamIndex).getName)
              }
              if (invalidRetType) {
                log(LogLevel.Warning, "User Defined Function ignored: %s. Unsupported return type: %s",
                  fcClassName, returnType)
              }
            } else {
              val evaluateParamTypes: List[NodeInfo.Kind] = initParamTypeConv.flatten.toList
              val Some(evaluateReturnType: NodeInfo.Kind) = initRetTypeConv

              val key = s"$fns:$fname"
              if (udfInfoLookup.contains(key)) {
                val Some(udfInfo) = udfInfoLookup.get(key)
                log(LogLevel.Warning, "User Defined Function ignored: %s. Duplicate %s from %s found.",
                  fcClassName, key, udfInfo.udfClass.getName)
              } else {
                val serializableEvaluate =
                  UserDefinedFunctionMethod(evaluateMethod.getDeclaringClass, evaluateMethodName, paramTypes)
                val emi = EvaluateMethodInfo(serializableEvaluate, evaluateParamTypes, evaluateReturnType)
                udfInfoLookup += (key -> UserDefinedFunctionInfo(fc, provider, emi))
              }

            }
          }
        }
      }
    }
  } catch {
    /*
     * We catch any errors thrown by the ServiceLoader here. This usually means UDFP
     * loading was disrupted, so no UDFs would be loaded
     */
    case e: ServiceConfigurationError =>
      val a = e.getCause.getStackTrace
      log(LogLevel.Error, "Error loading User Defined Function Providers: Error thrown in: %s", e.getCause)
  }

  if (allFunctionClasses.nonEmpty)
    log(LogLevel.Info, "User Defined Functions loaded: %s", UserDefinedFunctionService.allFunctionClasses)
  else
    log(LogLevel.Info, "No User Defined Functions loaded.")

  lazy val allFunctionClasses: String = udfInfoLookup.values.map {
    udfi =>
      s"${udfi.udfClass.getName()} => ${
        val a = udfi.udfClass.getAnnotation(classUserDefinedFunctionIdentification);
        a.namespaceURI() + ":" + a.name()
      }"
  }.mkString("\n")

  /**
   * Returns an initialized UserDefinedFunction object from the UserDefinedFunctionProvider
   * based on namespaceURI and function name from the schema
   *
   * @param namespaceUri The namespace associated with the schema function call
   * @param fname The function name from the schema function call
   *
   * @return Optional UserDefinedFunctionCallingInfo case class of initialized UserDefinedFunction
   * and its associated EvaluateMethodInfo consisting of its evaluate Method in a serializable
   * wrapper class, a list of the evaluate method's parameter types converted to NodeInfo.Kind and
   * the evaluate method's return type converted to NodeInfo.Kind
   *
   */
  def lookupUserDefinedFunctionCallingInfo(namespaceURI: String, fname: String): Option[UserDefinedFunctionCallingInfo] = {
    val udfid = s"$namespaceURI:$fname"

    val udfFunctionCallingInfo = initializedUserDefinedFunctionsCache.getOrElse(udfid,
      {
        val maybeUdfInfo = udfInfoLookup.get(udfid)
        if (maybeUdfInfo.isDefined) {
          val Some(udfInfo) = maybeUdfInfo

          val maybeUdf =
            try {
              val udf = udfInfo.provider.createUserDefinedFunction(namespaceURI, fname)
              Some(udf)
            } catch {
              case e: ReflectiveOperationException => {
                log(LogLevel.Error, "Error initializing User Defined Function: %s. Error thrown: %s",
                  udfid, e.getCause)
                None
              }
            }

          if (maybeUdf.isDefined) {
            val Some(udf) = maybeUdf
            val expectedUdfClass = udfInfo.udfClass
            val actualUdfClass = udf.getClass
            if (actualUdfClass != expectedUdfClass) {
              log(LogLevel.Error, "User Defined Function Class Mismatch: %s. Expected: %s Actual: %s",
                udfid, expectedUdfClass, actualUdfClass)
              None
            } else {
              val udfci = Some(UserDefinedFunctionCallingInfo(udf, udfInfo.evaluateMethodInfo))
              initializedUserDefinedFunctionsCache += (udfid -> udfci)
              udfci
            }
          } else {
            None
          }
        } else {
          None
        }
      })
    udfFunctionCallingInfo
  }

}
