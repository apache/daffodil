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

abstract class LayerException(msg: String, cause: Throwable) extends Exception(msg, cause) {
  def this(msg: String) = this(msg, null)
  def this(cause: Throwable) = this(null, cause)
}

class LayerCompilerException(msg: String, cause: Throwable) extends LayerException(msg, cause) {
  def this(msg: String) = this(msg, null)
  def this(cause: Throwable) = this(null, cause)
}

abstract class LayerRuntimeException(message: String, cause: Throwable)
  extends RuntimeException(message, cause) {
  def this(message: String) = this(message, null)
  def this(cause: Throwable) = this(null, cause)
}

class LayerNotEnoughDataException(needed: Int, available: Int)
  extends LayerRuntimeException(
    s"Insufficient data for layer. Needed $needed bytes, but only $available were available.",
  )

class LayerUnexpectedException(layerName: String, cause: Throwable)
  extends LayerRuntimeException(
    s"Unexpected exception in layer transformer '$layerName'",
    cause,
  )
