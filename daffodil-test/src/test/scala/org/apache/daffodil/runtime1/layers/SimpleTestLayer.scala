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

package org.apache.daffodil.runtime1.layers

// different subpackage on purpose to test package private methods

import java.io.InputStream
import java.io.OutputStream
import org.apache.daffodil.runtime1.layers.api.Layer

abstract class TLayer(n: String, ns: String) extends Layer(n, ns) {
  protected var intVar: Int = 0
  protected var stringVar: String = null
  override def wrapLayerInput(jis: InputStream): InputStream = jis
  override def wrapLayerOutput(jos: OutputStream): OutputStream = jos
}

final class STL_Ok1() extends TLayer("stlOk1", "urn:STL") {
  private[layers] def setLayerVariableParameters(intVar: Int, stringVar: String): Unit = {
    this.intVar = intVar
    this.stringVar = stringVar
  }
//  def getLayerVariableResult_intVar(): Int = intVar + intVar
//  def getLayerVariableResult_stringVar(): String = stringVar + " " + stringVar
}

final class STL_Ok2() extends TLayer("stlOk2", "urn:STL") {
  private[layers] def setLayerVariableParameters(): Unit = {
    this.intVar = 42
    this.stringVar = "forty two"
  }
  def getLayerVariableResult_intVar(): Int = intVar + intVar
  def getLayerVariableResult_stringVar(): String = stringVar + " " + stringVar
}

final class STL_Ok3() extends TLayer("stlOk3", "urn:STL") {
  private[layers] def setLayerVariableParameters(intVar: Int): Unit = {
    this.intVar = intVar
    this.stringVar = "forty two"
  }
  //  def getLayerVariableResult_intVar(): Int = intVar + intVar
  def getLayerVariableResult_stringVar(): String = stringVar + " " + stringVar
}

final class STL_Ok4() extends TLayer("stlOk4", "urn:STL") {
  private[layers] def setLayerVariableParameters(stringVar: String): Unit = {
    this.intVar = 42
    this.stringVar = stringVar
  }
  def getLayerVariableResult_intVar(): Int = intVar + intVar
  //  def getLayerVariableResult_stringVar(): String = stringVar + " " + stringVar
}
