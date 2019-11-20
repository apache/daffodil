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
package org.sbadudfs.functionclasses2.StringFunctions

import org.apache.daffodil.udf.UserDefinedFunction
import org.apache.daffodil.udf.UserDefinedFunctionProvider
import org.apache.daffodil.udf.UserDefinedFunctionIdentification
import java.io.Serializable

/**
 * UDF Provider for Negative Unit test
 *
 * passes non serializable or serializable state member to UDF
 */
class StringFunctionsProvider extends UserDefinedFunctionProvider {
  val nonSerializable = new SomeNonSerializableClass
  val serializable = new SomeSerializableClass

  override def getUserDefinedFunctionClasses = {
    Array(classOf[GetNonSerializableState], classOf[GetSerializableState])
  }

  override def createUserDefinedFunction(namespaceURI: String, fName: String) = {
    val udf = s"$namespaceURI:$fName" match {
      case "http://example.com/scala/udf:get-nonserializable-state" => new GetNonSerializableState(nonSerializable)
      case "http://example.com/scala/udf:get-serializable-state" => new GetSerializableState(serializable)
    }
    udf
  }
}

/**
 * Non serializable class for serialization negative tests in Scala
 */
class SomeNonSerializableClass {
  val x = "Nonserializable State"
}

/**
 * Serializable class for serialization tests in Scala
 */
class SomeSerializableClass extends Serializable {
  val x = "Serializable State"
}

/**
 * UDF for serialization negative tests in Scala
 *
 * contains a non serializable member
 */
@UserDefinedFunctionIdentification(
  name = "get-nonserializable-state",
  namespaceURI = "http://example.com/scala/udf")
class GetNonSerializableState(state: SomeNonSerializableClass) extends UserDefinedFunction {

  def evaluate() = {
    val ret = state.x
    ret
  }
}

/**
 * UDF for serialization tests in Scala
 *
 * contains a serializable member
 */
@UserDefinedFunctionIdentification(
  name = "get-serializable-state",
  namespaceURI = "http://example.com/scala/udf")
class GetSerializableState(state: SomeSerializableClass) extends UserDefinedFunction {

  def evaluate() = {
    val ret = state.x
    ret
  }
}
