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

package org.apache.daffodil.layers.runtime1

import java.io.InputStream
import java.io.OutputStream

import org.apache.daffodil.runtime1.layers.api.Layer

final class Base64MimeLayer
  extends Layer("base64_MIME", "urn:org.apache.daffodil.layers.base64_MIME") {

  override def wrapLayerOutput(jos: OutputStream): OutputStream =
    java.util.Base64.getMimeEncoder().wrap(jos)

  override def wrapLayerInput(jis: InputStream): InputStream =
    java.util.Base64.getMimeDecoder.wrap(jis)
}
