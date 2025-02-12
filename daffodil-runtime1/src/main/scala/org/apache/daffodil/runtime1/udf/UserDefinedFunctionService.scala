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

import java.io.ByteArrayOutputStream
import java.io.NotSerializableException
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.Serializable
import java.lang.reflect.Method
import java.util.ServiceConfigurationError
import java.util.ServiceLoader
import scala.collection.mutable._
import scala.collection.compat.immutable.ArraySeq

import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.udf._

/**
 * Loads, validates and caches (for use at schema compile time) all User Defined Functions
 * and their Providers from the classpath.
 *
 */
object UserDefinedFunctionService {

  lazy val evaluateMethodName = "evaluate"

  /**
   * Class to make the evaluate Method serializable and to optimize our reflection lookups
   * to happen once (per call) during deserialization, rather than at evaluation time.
   */
  case class UserDefinedFunctionMethod(
    val decClass: Class[_],
    val methodName: String,
    val paramTypes: Array[Class[_]]
  ) extends Serializable {

    @transient lazy val method: Method = {
      lookupMethod()
    }

    def lookupMethod() = {
      val m = decClass.getMethod(methodName, ArraySeq.unsafeWrapArray(paramTypes): _*)
      m
    }

    @throws(classOf[java.io.IOException])
    @throws(classOf[java.lang.ClassNotFoundException])
    private def readObject(in: ObjectInputStream): Unit = {
      in.defaultReadObject()
      this.method
    }

  }
  case class EvaluateMethodInfo(
    evaluateMethod: UserDefinedFunctionMethod,
    parameterTypes: List[NodeInfo.Kind],
    returnType: NodeInfo.Kind
  )
  case class UserDefinedFunctionCallingInfo(
    udf: UserDefinedFunction,
    evalInfo: EvaluateMethodInfo
  )
  case class UserDefinedFunctionInfo(
    udfClass: Class[_],
    provider: UserDefinedFunctionProvider,
    evaluateMethodInfo: EvaluateMethodInfo
  )
  type NamespaceUriAndName = String

  private val udfInfoLookup: HashMap[NamespaceUriAndName, UserDefinedFunctionInfo] = HashMap()
  private val initializedUserDefinedFunctionsCache
    : HashMap[NamespaceUriAndName, Option[UserDefinedFunctionCallingInfo]] = HashMap()

  lazy val classUserDefinedFunctionIdentification = classOf[UserDefinedFunctionIdentification]
  lazy val classUserDefinedFunction = classOf[UserDefinedFunction]
  lazy val currentClassPath = Misc.classPath.map(_.toString).mkString("\n")

  {
    //
    // Note that this service loader does not share the SimpleNamedServiceLoader with the loadable validators
    // and layering features. UDFs are not quite so simple, so the UDF system has its own implementation of service loading.
    //
    val loader: ServiceLoader[UserDefinedFunctionProvider] =
      ServiceLoader.load(classOf[UserDefinedFunctionProvider])

    val providerIter = loader.iterator()

    while (providerIter.hasNext()) {

      val providerOpt =
        try {
          Some(providerIter.next())
        } catch {
          case e: ServiceConfigurationError => {
            Logger.log.warn(s"User Defined Function Provider failed to load: ${e.getMessage}")
            None
          }
        }

      val providerFunctionClasses = providerOpt
        .map { provider =>
          try {
            val functionClasses = provider.getUserDefinedFunctionClasses
            if (functionClasses == null || functionClasses.isEmpty) {
              Logger.log.warn(
                s"User Defined Function Provider ignored: ${provider.getClass.getName}. No User Defined Functions found."
              )
              Seq()
            } else {
              functionClasses.toSeq
            }
          } catch {
            /*
             * This is to protect against any errors thrown when we are trying to load the
             * UDFs. It will catch any exceptions and emit them as the reason the UDF was
             * dropped
             */
            case e: Exception => {
              Logger.log.warn(
                s"User Defined Function Provider ignored: ${provider.getClass.getName}. Error loading User Defined Functions: ${e}"
              )
              Seq()
            }
          }
        }
        .getOrElse(Seq())

      val goodFunctionClasses = providerFunctionClasses.filter { udfc =>
        val nonAnn = !udfc.isAnnotationPresent(classUserDefinedFunctionIdentification)
        val nonUdf = !classUserDefinedFunction.isAssignableFrom(udfc)
        if (nonAnn) {
          Logger.log.warn(
            s"User Defined Function ignored: ${udfc.getName}. Missing ${classUserDefinedFunctionIdentification.getName} annotation"
          )
        }
        if (nonUdf) {
          Logger.log.warn(
            s"User Defined Function ignored: ${udfc.getName}. Doesn't implement ${classUserDefinedFunction.getName}"
          )
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
          Logger.log.warn(
            s"User Defined Function ignored: ${fcClassName}. Annotation namespace field is empty or invalid."
          )
        } else if (Misc.isNullOrBlank(fname)) {
          Logger.log.warn(
            s"User Defined Function ignored: ${fcClassName}. Annotation name field is empty or invalid."
          )
        } else if (evalMethods.isEmpty) {
          Logger.log.warn(
            s"User Defined Function ignored: ${fcClassName}. Missing evaluate method: ${fns}:${fname}"
          )
        } else if (evalMethods.length > 1) {
          Logger.log.warn(
            s"User Defined Function ignored: ${fcClassName}. Overloaded evaluate method: ${fns}:${fname}"
          )
        } else {
          val evaluateMethod = evalMethods.head
          val paramTypes = evaluateMethod.getParameterTypes
          val returnType = evaluateMethod.getReturnType

          val initParamTypeConv = paramTypes.zipWithIndex.map { case (c, i) =>
            NodeInfo.fromClass(c) -> i
          }
          val initRetTypeConv = NodeInfo.fromClass(returnType)
          val badParams = initParamTypeConv
            .collect { case (t, i) if t.isEmpty => val cn = paramTypes(i); cn.getSimpleName }

          if (badParams.nonEmpty) {
            Logger.log.warn(
              s"User Defined Function ignored: ${fcClassName}. Unsupported parameter type(s): ${badParams
                  .mkString(",")}"
            )
          } else if (initRetTypeConv.isEmpty) {
            Logger.log.warn(
              s"User Defined Function ignored: ${fcClassName}. Unsupported return type: ${returnType.getSimpleName}"
            )
          } else {
            val evaluateParamTypes: List[NodeInfo.Kind] = initParamTypeConv.flatMap {
              _._1
            }.toList
            val Some(evaluateReturnType: NodeInfo.Kind) = initRetTypeConv

            val key = s"{$fns}$fname"
            if (udfInfoLookup.contains(key)) {
              val udfInfo = udfInfoLookup(key)
              Logger.log.warn(
                s"User Defined Function ignored: ${fcClassName}. Duplicate ${key} from ${udfInfo.udfClass.getName} found."
              )
            } else {
              val serializableEvaluate =
                UserDefinedFunctionMethod(
                  evaluateMethod.getDeclaringClass,
                  evaluateMethodName,
                  paramTypes
                )
              val emi =
                EvaluateMethodInfo(serializableEvaluate, evaluateParamTypes, evaluateReturnType)
              udfInfoLookup += (key -> UserDefinedFunctionInfo(fc, providerOpt.get, emi))
              Logger.log.debug(s"User Defined Function loaded: ${fcClassName} => ${key}")
            }
          }
        }
      }
    }
  }

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
  def lookupUserDefinedFunctionCallingInfo(
    namespaceURI: String,
    fname: String
  ): Option[UserDefinedFunctionCallingInfo] = {
    val udfid = s"{$namespaceURI}$fname"

    val udfFunctionCallingInfo = initializedUserDefinedFunctionsCache.getOrElse(
      udfid, {
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
                  Logger.log.warn(
                    s"User Defined Function is not serializable: ${udf.getClass.getName}. Could not serialize member of class: ${e.getMessage}"
                  )
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
                throw new UserDefinedFunctionFatalErrorException(
                  s"User Defined Function could not be initialized: ${udfid}",
                  actualCause,
                  udfInfo.udfClass.getName,
                  udfInfo.provider.getClass.getName
                )
              }
            }

          maybeUdf.flatMap { udf =>
            val expectedUdfClass = udfInfo.udfClass
            val actualUdfClass = udf.getClass
            if (actualUdfClass != expectedUdfClass) {
              Logger.log.warn(
                s"User Defined Function class mismatch: ${udfid}. Expected: ${expectedUdfClass} Actual: ${actualUdfClass}"
              )
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
      }
    )
    udfFunctionCallingInfo
  }

}
