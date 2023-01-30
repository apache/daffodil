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

package org.apache.daffodil.layers

import java.io.InputStream
import org.apache.daffodil.runtime1.layers.LayerCompiler
import org.apache.daffodil.runtime1.layers.LayerCompileInfo
import org.apache.daffodil.runtime1.layers.LayerRuntimeInfo
import org.apache.daffodil.runtime1.layers.LayerTransformer
import org.apache.daffodil.runtime1.layers.LayerTransformerFactory
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState


final class BuggyLayerCompiler
  extends LayerCompiler("buggy") {

  override def compileLayer(layerCompileInfo: LayerCompileInfo): BuggyTransformerFactory = {
    new BuggyTransformerFactory(name)
  }
}

final class BuggyTransformerFactory(name: String)
  extends LayerTransformerFactory(name) {

  override def newInstance(layerRuntimeInfo: LayerRuntimeInfo)= {
    new BuggyTransformer(name, layerRuntimeInfo)
  }
}

class BuggyTransformer(name: String, layerRuntimeInfo: LayerRuntimeInfo)
  extends LayerTransformer(name, layerRuntimeInfo) {

  override def wrapLayerDecoder(jis: InputStream) = {
    new BuggyInputStream(jis)
  }

  override def wrapLimitingStream(state: ParseOrUnparseState, jis: InputStream) = {
    jis
  }

  override protected def wrapLayerEncoder(jos: java.io.OutputStream): java.io.OutputStream = {
    jos
  }

  override protected def wrapLimitingStream(state: ParseOrUnparseState, jis: java.io.OutputStream) = {
    jis
  }
}

final class BuggyInputStream(is: InputStream) extends InputStream {

  def read(): Int = {
    val b = is.read()
    if (b != '0') b else throw new java.io.IOException("bad input stream")
  }

}
