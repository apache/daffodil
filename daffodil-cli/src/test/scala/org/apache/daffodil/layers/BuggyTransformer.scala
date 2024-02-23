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
import java.io.OutputStream

import org.apache.daffodil.runtime1.layers.api.Layer
import org.apache.daffodil.runtime1.layers.api.LayerRuntime

final class BuggyLayer extends Layer("buggy") {

  override def wrapLayerDecoder(jis: InputStream, lr: LayerRuntime): InputStream =
    new BuggyInputStream(jis)

  override def wrapLayerEncoder(jos: OutputStream, lr: LayerRuntime): OutputStream =
    jos
}

final class BuggyInputStream(is: InputStream) extends InputStream {

  def read(): Int = {
    val b = is.read()
    if (b != '0') b else throw new java.io.IOException("bad input stream")
  }

}
