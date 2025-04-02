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

package org.apache.daffodil.io

import org.junit.Assert._
import org.junit.Test

class TestInputSourceDataInputStream3 {

  val Dump = new DataDumper

  @Test def dumpVisible1(): Unit = {
    val bytes = "Date 年月日=2003年08月27日".getBytes("utf-8")
    val lengthInBits = bytes.length * 8
    val dis = InputSourceDataInputStream(bytes)
    dis.setDebugging(true)
    val fb = dis.futureData(48)

    val dumpString =
      Dump
        .dump(Dump.MixedHexLTR(Some("utf-8")), 0, lengthInBits, fb, includeHeadingLine = true)
        .mkString("\n")
    val expected = """
87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0~1~2~3~4~5~6~7~8~9~a~b~c~d~e~f~
00000000: 4461 7465 20e5 b9b4 e69c 88e6 97a5 3d32  D~a~t~e~␣~年~~~~月~~~~日~~~~=~2~
00000010: 3030 33e5 b9b4 3038 e69c 8832 37e6 97a5  0~0~3~年~~~~0~8~月~~~~2~7~日~~~~
""".replace("\r\n", "\n")
    assertEquals(expected, "\n" + dumpString + "\n")
  }

  @Test def dumpVisible2(): Unit = {
    val bytes = "Date 年月日=2003年08月27日".getBytes("utf-8")
    val lengthInBits = bytes.length * 8
    val dis = InputSourceDataInputStream(bytes)
    dis.setDebugging(true)
    val fb = dis.futureData(48)

    val dumpString =
      Dump
        .dump(
          Dump.MixedHexLTR(Some("utf-8")),
          16 * 8,
          lengthInBits,
          fb,
          includeHeadingLine = true
        )
        .mkString("\n")
    val expected = """
87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0~1~2~3~4~5~6~7~8~9~a~b~c~d~e~f~
00000010: 4461 7465 20e5 b9b4 e69c 88e6 97a5 3d32  D~a~t~e~␣~年~~~~月~~~~日~~~~=~2~
00000020: 3030 33e5 b9b4 3038 e69c 8832 37e6 97a5  0~0~3~年~~~~0~8~月~~~~2~7~日~~~~
""".replace("\r\n", "\n")

    assertEquals(expected, "\n" + dumpString + "\n")
  }

  @Test def dumpVisible3(): Unit = {
    val bytes = "Date 年月日=2003年08月27日".getBytes("utf-8")
    val lengthInBits = bytes.length * 8
    val dis = InputSourceDataInputStream(bytes)
    dis.setDebugging(true)
    val fb = dis.futureData(bytes.length)

    val dumpString =
      Dump
        .dump(
          Dump.MixedHexLTR(Some("utf-8")),
          20 * 8,
          lengthInBits,
          fb,
          includeHeadingLine = true
        )
        .mkString("\n")
    val expected = """
87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0~1~2~3~4~5~6~7~8~9~a~b~c~d~e~f~
00000010:           4461 7465 20e5 b9b4 e69c 88e6          D~a~t~e~␣~年~~~~月~~~~日
00000020: 97a5 3d32 3030 33e5 b9b4 3038 e69c 8832  ~~~~=~2~0~0~3~年~~~~0~8~月~~~~2~
00000030: 37e6 97a5                                7~日~~~~                        
""".replace("\r\n", "\n")
    assertEquals(expected, "\n" + dumpString + "\n")
  }
}
