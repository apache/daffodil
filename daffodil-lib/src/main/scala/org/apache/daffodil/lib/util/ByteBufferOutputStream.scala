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
package org.apache.daffodil.lib.util

import java.io.OutputStream
import java.nio.ByteBuffer

/**
 * Provides OutputStream API, buf fills in a finite-sized ByteBuffer.
 *
 * Overflowing the capacity of the byte buffer throws an exception.
 *
 * @param bb The ByteBuffer to be filled in.
 */
class ByteBufferOutputStream(val byteBuffer: ByteBuffer) extends OutputStream {

  byteBuffer.limit(byteBuffer.capacity())

  def write(b: Int): Unit = byteBuffer.put(b.toByte)

  def size(): Int = byteBuffer.position()
}
