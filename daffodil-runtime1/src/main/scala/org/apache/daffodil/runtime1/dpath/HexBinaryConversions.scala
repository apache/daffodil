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

package org.apache.daffodil.runtime1.dpath

object HexBinaryConversions {

  def toByteArray(b: Byte): Array[Byte] = Array(b)
  def toByteArray(s: Short): Array[Byte] = {
    val upper = ((s >> 8) & 0x00ff).toByte
    val lower = (s & 0x00ff).toByte
    Array[Byte](upper, lower)
  }
  def toByteArray(i: Int): Array[Byte] = {
    val byte0 = ((i >> 24) & 0x000000ff).toByte
    val byte1 = ((i >> 16) & 0x000000ff).toByte
    val byte2 = ((i >> 8) & 0x000000ff).toByte
    val byte3 = (i & 0x000000ff).toByte

    Array[Byte](byte0, byte1, byte2, byte3)
  }
  def toByteArray(l: Long): Array[Byte] = {
    val i0: Integer = ((l >> 32) & 0xffffffff).toInt
    val i1: Integer = (l & 0xffffffff).toInt
    val arr0 = toByteArray(i0)
    val arr1 = toByteArray(i1)

    arr0 ++ arr1
  }

}
