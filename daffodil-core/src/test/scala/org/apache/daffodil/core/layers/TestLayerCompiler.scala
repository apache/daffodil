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
package org.apache.daffodil.core.layers

import java.io.InputStream
import java.io.OutputStream

import org.apache.daffodil.runtime1.layers.LayerFactory
import org.apache.daffodil.runtime1.layers.api.Layer
import org.apache.daffodil.runtime1.layers.api.LayerRuntime

import org.junit.Test

class TestLayerCompiler {

  @Test def testClassAnalyzer1(): Unit = {
    val constructorInfo = LayerFactory.analyzeClass(new LayerForTesting)
    println(constructorInfo)
  }
}

class LayerForTesting(
  firstIntArg: java.lang.Integer,
  secondStringArg: String,
  thirdDateArg: com.ibm.icu.util.Calendar,
) extends Layer("test", "urn:layerForTesting") {

  def this() = this(null, null, null)

  override def wrapLayerInput(jis: InputStream, lr: LayerRuntime): InputStream = jis

  override def wrapLayerOutput(jos: OutputStream, lr: LayerRuntime): OutputStream = jos
}
