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

package org.apache.daffodil.sapi.io

import java.io.InputStream
import java.nio.ByteBuffer

import org.apache.daffodil.io.{ InputSourceDataInputStream => SInputSourceDataInputStream }

/**
 * Provides Daffodil with byte data from an InputStream, ByteBuffer, or byte
 * Array.
 *
 * @param dis the underlying Scala InputSourceDataInputStream
 */
class InputSourceDataInputStream private[sapi] (private [sapi] val dis: SInputSourceDataInputStream) {

  /**
   * Create an InputSourceDataInputStream from a java.io.InputStream
   */
  def this(is: InputStream) = this(SInputSourceDataInputStream(is))

  /**
   * Create an InputSourceDataInputStream from a java.nio.ByteBuffer
   */
  def this(bb: ByteBuffer) = this(SInputSourceDataInputStream(bb)) 

  /**
   * Create an InputSourceDataInputStream from a byte array
   */
  def this(arr: Array[Byte]) = this(SInputSourceDataInputStream(arr)) 
}
