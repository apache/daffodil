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
import java.io.Serializable
import java.io.ObjectInputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream
import java.io.NotSerializableException

/**
 * Loads, validates and caches (for use at schema compile time) all User Defined Functions
 * and their Providers from the classpath.
 *
 */
object UserDefinedFunctionService extends Logging {
  lazy val evaluateMethodName = "evaluate"

  /**
   * Class to make the evaluate Method serializable and to optimize our reflection lookups
   * to happen once (per call) during deserialization, rather than at evaluation time.
   */
  case class UserDefinedFunctionMethod(
    val decClass: Class[_],
    val methodName: String,
    val paramTypes: Array[Class[_]])
    extends Serializable {

    @transient lazy val method: Method = {
      lookupMethod
    }

    def lookupMethod() = {
      val m = decClass.getMethod(methodName, paramTypes: _*)
      m
    }

    @throws(classOf[java.io.IOException])
    @throws(classOf[java.lang.ClassNotFoundException])
    private def readObject(in: ObjectInputStream): Unit = {
      in.defaultReadObject()
      this.method
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

  try {
    val loader: ServiceLoader[UserDefinedFunctionProvider] = ServiceLoader.load(classOf[UserDefinedFunctionProvider])
    loader.asScala.foreach { provider =>
      lazy val providerClassName = provider.getClass.getName
      val possibleProviderFunctionClasses =
        /*
         * This is to protect against any errors thrown when we are trying to load the
         * UDFs. It will catch any exceptions and emit them as the reason the UDF was
         * dropped
         */
        try {
          Option(provider.getUserDefinedFunctionClasses)
        } catch {
          case e: Exception =>
            log(
              LogLevel.Warning,
              "User Defined Function Provider ignored: %s. Error loading User Defined Functions: %s",
              providerClassName, e)
            None
        }

      if (possibleProviderFunctionClasses.isEmpty || possibleProviderFunctionClasses.get.isEmpty) {
        log(LogLevel.Warning, "User Defined Function Provider ignored: %s. No User Defined Functions found.",
          providerClassName)
      } else {
        val Some(providerFunctionClasses) = possibleProviderFunctionClasses
        lazy val goodFunctionClasses = providerFunctionClasses.filter {
          udfc =>
            val nonAnn = !udfc.isAnnotationPresent(classUserDefinedFunctionIdentification)
            val nonUdf = !classUserDefinedFunction.isAssignableFrom(udfc)
            if (nonAnn) {
              log(LogLevel.Warning, "User Defined Function ignored: %s. Missing %s annotation",
                udfc.getName, classUserDefinedFunctionIdentification.getName)
            }
            if (nonUdf) {
              log(LogLevel.Warning, "User Defined Function ignored: %s. Doesn't implement %s",
                udfc.getName, classUserDefinedFunction.getName)
            }
            val ret = !(nonAnn || nonUdf)
            ret
        }

        goodFunctionClasses.foreach { fc =>
          val fcClassName = fc.getName
          val fInfo = fc.getDeclaredAnnotation(classUserDefinedFunctionIdentification)
          val fns = fInfo.namespaceURI()
          val fname = fInfo.name()
          val evalMethods = fc.getMethods.filter(_.getName == evaluateMethodName)

          if (Misc.isNullOrBlank(fns)) {
            log(
              LogLevel.Warning,
              "User Defined Function ignored: %s. Annotation namespace field is empty or invalid.",
              fcClassName)
          } else if (Misc.isNullOrBlank(fname)) {
            log(
              LogLevel.Warning,
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

            val initParamTypeConv = paramTypes.zipWithIndex.map { case (c, i) => NodeInfo.fromClass(c) -> i }
            val initRetTypeConv = NodeInfo.fromClass(returnType)
            val badParams = initParamTypeConv
              .collect { case (t, i) if t.isEmpty => val cn = paramTypes(i); cn.getSimpleName }

            if (badParams.nonEmpty) {
              log(LogLevel.Warning, "User Defined Function ignored: %s. Unsupported parameter type(s): %s",
                fcClassName, badParams.mkString(","))
            } else if (initRetTypeConv.isEmpty) {
              log(LogLevel.Warning, "User Defined Function ignored: %s. Unsupported return type: %s",
                fcClassName, returnType.getSimpleName)
            } else {
              val evaluateParamTypes: List[NodeInfo.Kind] = initParamTypeConv.flatMap { _._1 }.toList
              val Some(evaluateReturnType: NodeInfo.Kind) = initRetTypeConv

              val key = s"$fns:$fname"
              if (udfInfoLookup.contains(key)) {
                val udfInfo = udfInfoLookup(key)
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
      log(LogLevel.Error, "Error while loading User Defined Function Providers: %s", e.getMessage)
      log(LogLevel.Debug, "Current Classpath: %s", currentClassPath)
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

    val udfFunctionCallingInfo = initializedUserDefinedFunctionsCache.getOrElse(
      udfid,
      {
        val maybeUdfInfo = udfInfoLookup.get(udfid)
        maybeUdfInfo.flatMap { udfInfo =>
          val maybeUdf =
            try {
              val udf = udfInfo.provider.createUserDefinedFunction(namespaceURI, fname)
              /*
               * This is to check for any errors thrown when if we try to serialize the
               * UDF, such as when using save-parser
               */
              try {
                new ObjectOutputStream(new ByteArrayOutputStream()).writeObject(udf)
                Option(udf)
              } catch {
                case e: NotSerializableException =>
                  log(LogLevel.Error, "Error serializing initialized User Defined Function: %s. Could not serialize member of class: %s",
                    udf.getClass.getName, e.getMessage)
                  None
              }
            } catch {
              /*
               * This is to protect against any errors thrown when we are trying to
               * initialize the UDFs. It will catch any exceptions and emit them as the reason the UDF
               * is unsupported
               */
              case e: Exception => {
                val actualCause = e match {
                  case _: ReflectiveOperationException => e.getCause
                  case x => x
                }
                log(LogLevel.Error, "Error initializing User Defined Function: %s. Error thrown: %s",
                  udfid, actualCause)
                throw new UserDefinedFunctionFatalErrorException(
                  s"User Defined Function '$udfid' Error",
                  actualCause, udfInfo.udfClass.getName, udfInfo.provider.getClass.getName)
              }
            }

          maybeUdf.flatMap { udf =>
            val expectedUdfClass = udfInfo.udfClass
            val actualUdfClass = udf.getClass
            if (actualUdfClass != expectedUdfClass) {
              log(LogLevel.Error, "User Defined Function Class Mismatch: %s. Expected: %s Actual: %s",
                udfid, expectedUdfClass, actualUdfClass)
              None
            } else {
              val udfInfoEval = udfInfo.evaluateMethodInfo
              /*
               * We "call" .method here to force the reflection lookup of the method at
               * compile time, and to minimize this cost at run time.
               *
               * It is acknowledged that the initialization done here is thrown out on
               * serialization, and redone at deserialization, and we are ok with it.
               */
              udfInfoEval.evaluateMethod.method
              val udfci = Some(UserDefinedFunctionCallingInfo(udf, udfInfo.evaluateMethodInfo))
              initializedUserDefinedFunctionsCache += (udfid -> udfci)
              udfci
            }
          }
        }
      })
    udfFunctionCallingInfo
  }

}
