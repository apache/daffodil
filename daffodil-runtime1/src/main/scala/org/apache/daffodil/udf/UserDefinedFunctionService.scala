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
import java.io.Serializable
import org.apache.daffodil.util.LogLevel
import org.apache.daffodil.util.Logging
import java.lang.reflect.Method

/**
 * Loads all User Defined Function Providers on the classpath once and caches them in
 * following look up tables that'll be used to look up the UDFs during Schema compile time.
 *
 * "$namespaceURI:$fName" -> classOf[UserDefinedFunction]
 * "$namespaceURI:$fName" -> UserDefinedFunctionProvider
 *
 * It also does validation on the providers and classes and caches any warning and errors,
 * which are then reported during compile time.
 *
 */
object UserDefinedFunctionService extends Logging {
  val evaluateMethodName = "evaluate"
  type EvaluateMethod = Method
  type ParameterTypes = Array[Class[_]]
  type ReturnType = Class[_]
  type UdfCallingInfo = (UserDefinedFunction, EvaluateMethod, ParameterTypes, ReturnType)
  type NamespaceNameKey = String
  private val functionClassesLookup: HashMap[NamespaceNameKey, Class[_]] = HashMap()
  private val functionProviderLookup: HashMap[NamespaceNameKey, UserDefinedFunctionProvider] = HashMap()
  private val InitializedUserDefinedFunctionsCache: HashMap[NamespaceNameKey, Option[UdfCallingInfo]] = HashMap()
  lazy val classUserDefinedFunctionInfo = classOf[UserDefinedFunctionInfo]
  lazy val classUserDefinedFunction = classOf[UserDefinedFunction]
  lazy val classJSerializable = classOf[java.io.Serializable]
  lazy val currentClassPath = Misc.classPath.map(_.toString).mkString("\n")

  val loader: ServiceLoader[UserDefinedFunctionProvider] = ServiceLoader.load(classOf[UserDefinedFunctionProvider])

  loader.asScala.map { provider =>
    val providerfc = provider.getUserDefinedFunctionClasses
    lazy val providerClassName = provider.getClass.getName

    if (providerfc == null || providerfc.isEmpty) {
      val wstr = s"User Defined Function Provider ignored: ${providerClassName}. No User Defined Functions found."
      log(LogLevel.Warning, wstr)
    } else {
      val missingAnnotations = providerfc.filterNot(_.isAnnotationPresent(classUserDefinedFunctionInfo))
      val nonUserDefinedFunction = providerfc.filterNot(classUserDefinedFunction.isAssignableFrom(_))

      if (!missingAnnotations.isEmpty) {
        val wstr = s"User Defined Function Provider ignored: ${providerClassName}. Below must be annotated with ${classUserDefinedFunctionInfo.getName}" +
          s"\n${missingAnnotations.map(_.getName).mkString("\t", "\n\t", "")}"
        log(LogLevel.Warning, wstr)
      }

      if (!nonUserDefinedFunction.isEmpty) {
        val wstr = s"User Defined Function Provider ignored: ${providerClassName}. Below must implement ${classUserDefinedFunction.getName}" +
          s"\n${nonUserDefinedFunction.map(_.getName).mkString("\t", "\n\t", "")}"
        log(LogLevel.Warning, wstr)
      }

      if (missingAnnotations.isEmpty && nonUserDefinedFunction.isEmpty) {
        providerfc.map { fc =>
          val fcClassName = fc.getName
          val fInfo = fc.getDeclaredAnnotation(classUserDefinedFunctionInfo)
          val fns = fInfo.namespaceURI()
          val fname = fInfo.name()
          if (List(fns, fname).exists { p => Option(p).isEmpty || p.trim.isEmpty }) {
            val wstr = s"User Defined Function ignored: ${fcClassName} from provider ${providerClassName}." +
              (if (Option(fns).isEmpty || fns.trim.isEmpty) " Annotation namespace field is empty or invalid." else "") +
              (if (Option(fname).isEmpty || fname.trim.isEmpty) " Annotation namespace field is empty or invalid." else "")
            log(LogLevel.Warning, wstr)
          }
          val key = s"$fns:$fname"
          if (functionProviderLookup.contains(key)) {
            val estr = s"User Defined Function ignored: ${fcClassName} from provider ${providerClassName}. Duplicate $key found."
            log(LogLevel.Error, estr)
          } else {
            functionProviderLookup += (key -> provider)
            functionClassesLookup += (key -> fc)
          }
        }
      }
    }
  }

  if (functionClassesLookup.isEmpty) {
    log(LogLevel.Warning, "No user defined functions found. Check that UDF JARs are on classpath and that they are properly registerable by ServiceLoader.")
    log(LogLevel.Info, s"Current classpath locations:\n${currentClassPath}")
  }

  def getUserDefinedFunctionClasses = functionClassesLookup.values.toArray

  lazy val allFunctionClasses: String = getUserDefinedFunctionClasses.map {
    c => s"[${c.getName()} => ${c.getAnnotation(classUserDefinedFunctionInfo)}]"
  }.mkString("\n")

  /**
   * Returns an initialized UserDefinedFunction object from the UserDefinedFunctionProvider
   * based on namespaceURI and function name from the schema
   *
   * @return Optional Tuple of initialized UserDefinedFunction, evaluate Method, array of
   * the evaluate's parameter types and evaluate's return type
   */
  def lookupUDFCallingInfo(namespaceURI: String, fname: String): Option[UdfCallingInfo] = {
    val udfid = s"$namespaceURI:$fname"

    val udfFunctionCallingInfo = InitializedUserDefinedFunctionsCache.getOrElse(udfid, {
      val provider = functionProviderLookup.get(udfid)
      val fClass = functionClassesLookup.get(udfid)
      if (!List(provider, fClass).exists(_.isEmpty)) {
        val udf = provider.get.lookupInitializedUserDefinedFunction(namespaceURI, fname)
        val expectedUdfClass = fClass.get

        if (udf != null) {
          val actualUdfClass = udf.getClass
          if (actualUdfClass != expectedUdfClass) {
            log(LogLevel.Error, s"User Defined Function Class Mismatch: $udfid. Expected: $expectedUdfClass Actual: $actualUdfClass")
            None
          } else {

            val evaluateMethodInfoTuples = expectedUdfClass.getMethods.collect {
              case p if p.getName == evaluateMethodName => (p, p.getParameterTypes, p.getReturnType)
            }

            if (evaluateMethodInfoTuples.isEmpty) {
              log(LogLevel.Error, s"Missing evaluate method for function provided: $udfid")
              None
            } else if (evaluateMethodInfoTuples.length > 1) {
              log(LogLevel.Error, s"Only one evaluate method allowed per function class: $udfid")
              None
            } else if (List(classOf[java.lang.Void], classOf[Unit]).exists(_ == evaluateMethodInfoTuples.head._3)) {
              log(LogLevel.Error, s"Evaluate method for function provided cannot be void: $udfid")
              None
            } else {
              val evaluateMethodInfo: (EvaluateMethod, ParameterTypes, ReturnType) = evaluateMethodInfoTuples.head
              /*
               * Attempting assignnment by tuple matching results in a scala existentials error in SBT
               * so we do it this way instead
               */
              val evaluateMethod: EvaluateMethod = evaluateMethodInfo._1
              val paramTypes: ParameterTypes = evaluateMethodInfo._2
              val retType: ReturnType = evaluateMethodInfo._3
              val udfci: Option[UdfCallingInfo] = Some((udf, evaluateMethod, paramTypes, retType))
              InitializedUserDefinedFunctionsCache += (udfid -> udfci)
              udfci
            }
          }
        } else {
          log(LogLevel.Error, s"Function not found: $udfid")
          log(LogLevel.Info, s"Currently registered UDFs:\n${UserDefinedFunctionService.allFunctionClasses}")
          None
        }
      } else {
        val requiredThing = List(
          (if (provider.isEmpty) "Provider" else ""),
          (if (fClass.isEmpty) "User Defined Function" else ""))
          .filter(s => !Option(s).isEmpty && s.trim.nonEmpty).mkString("/")
        log(LogLevel.Error, s"$requiredThing not found: $udfid")
        None
      }
    })
    udfFunctionCallingInfo
  }

}
