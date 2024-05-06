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

import java.nio.ByteBuffer

import org.junit.Assert._
import org.junit.Test

class TestDump {

  val Dump = new DataDumper

  @Test def testDumpHexAndText1(): Unit = {

    val bytes = "Date 年月日=2003年08月27日".getBytes("utf-8")
    val lengthInBits = bytes.length * 8
    val bb = ByteBuffer.wrap(bytes)

    val dumpString =
      Dump
        .dump(Dump.MixedHexLTR(Some("utf-8")), 0, lengthInBits, bb, includeHeadingLine = true)
        .mkString("\n")
    val expected = """
87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0~1~2~3~4~5~6~7~8~9~a~b~c~d~e~f~
00000000: 4461 7465 20e5 b9b4 e69c 88e6 97a5 3d32  D~a~t~e~␣~年~~~~月~~~~日~~~~=~2~
00000010: 3030 33e5 b9b4 3038 e69c 8832 37e6 97a5  0~0~3~年~~~~0~8~月~~~~2~7~日~~~~
""".replace("\r\n", "\n")
    assertEquals(expected, "\n" + dumpString + "\n")
  }

  @Test def testDumpHexAndText1a(): Unit = {

    val dateString = "Date 年月日=2003年08月27日"
    val dateStringLengthInBytes = dateString.getBytes("utf-8").length
    val bytes = dateString.getBytes("utf-8")
    val bb = ByteBuffer.wrap(bytes)

    val dumpString =
      Dump
        .dump(
          Dump.MixedHexLTR(Some("utf-8")),
          1000 * 8,
          dateStringLengthInBytes * 8,
          bb,
          includeHeadingLine = true,
          indicatorInfo = Some(((1000 + 12) * 8), 6 * 8)
        )
        .mkString("\n")
    //
    // This is a bit hard to interpret. The indicator starts above a blank part
    // of the first row of data. That means it is pointing at the next row.
    //
    val expected = """
                    ├────────────┤                         ├──────────┤
87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0~1~2~3~4~5~6~7~8~9~a~b~c~d~e~f~
000003e0:                     4461 7465 20e5 b9b4                  D~a~t~e~␣~年~~~~
000003f0: e69c 88e6 97a5 3d32 3030 33e5 b9b4 3038  月~~~~日~~~~=~2~0~0~3~年~~~~0~8~
00000400: e69c 8832 37e6 97a5                      月~~~~2~7~日~~~~                
""".replace("\r\n", "\n")
    assertEquals(expected, "\n" + dumpString + "\n")
  }

  @Test def testDumpHexAndText1WithIndicator1(): Unit = {

    val bytes = "Date 年月日=2003年08月27日".getBytes("utf-8")
    val lengthInBits = bytes.length * 8
    val bb = ByteBuffer.wrap(bytes)

    val dumpString =
      Dump
        .dump(
          Dump.MixedHexLTR(Some("utf-8")),
          0,
          lengthInBits,
          bb,
          includeHeadingLine = true,
          indicatorInfo = Some((0, lengthInBits))
        )
        .mkString("\n")
    val expected = """
          ├─────────────────────────────────────═  ├──────────────────────────────═
87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0~1~2~3~4~5~6~7~8~9~a~b~c~d~e~f~
00000000: 4461 7465 20e5 b9b4 e69c 88e6 97a5 3d32  D~a~t~e~␣~年~~~~月~~~~日~~~~=~2~
00000010: 3030 33e5 b9b4 3038 e69c 8832 37e6 97a5  0~0~3~年~~~~0~8~月~~~~2~7~日~~~~
""".replace("\r\n", "\n")
    assertEquals(expected, "\n" + dumpString + "\n")
  }

  @Test def testDumpHexAndText1WithIndicator2(): Unit = {

    val bytes = "Date 年月日=2003年08月27日".getBytes("utf-8")
    val lengthInBits = bytes.length * 8
    val bb = ByteBuffer.wrap(bytes)

    val dumpString =
      Dump
        .dump(
          Dump.MixedHexLTR(Some("utf-8")),
          0,
          lengthInBits,
          bb,
          includeHeadingLine = true,
          indicatorInfo = Some((8, 14 * 8))
        )
        .mkString("\n")
    val expected = """
            ├─────────────────────────────────┤      ├──────────────────────────┤
87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0~1~2~3~4~5~6~7~8~9~a~b~c~d~e~f~
00000000: 4461 7465 20e5 b9b4 e69c 88e6 97a5 3d32  D~a~t~e~␣~年~~~~月~~~~日~~~~=~2~
00000010: 3030 33e5 b9b4 3038 e69c 8832 37e6 97a5  0~0~3~年~~~~0~8~月~~~~2~7~日~~~~
""".replace("\r\n", "\n")
    assertEquals(expected, "\n" + dumpString + "\n")
  }

  @Test def testDumpHexAndText2(): Unit = {

    val bytes = "Date 年月日=2003年08月27日".getBytes("utf-32BE")
    val lengthInBits = bytes.length * 8
    val bb = ByteBuffer.wrap(bytes)

    val dumpString = Dump
      .dump(Dump.MixedHexLTR(Some("utf-32BE")), 0, lengthInBits, bb, includeHeadingLine = true)
      .mkString("\n")
    val expected = """
87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0~1~2~3~4~5~6~7~8~9~a~b~c~d~e~f~
00000000: 0000 0044 0000 0061 0000 0074 0000 0065  D~~~~~~~a~~~~~~~t~~~~~~~e~~~~~~~
00000010: 0000 0020 0000 5e74 0000 6708 0000 65e5  ␣~~~~~~~年~~~~~~月~~~~~~日~~~~~~
00000020: 0000 003d 0000 0032 0000 0030 0000 0030  =~~~~~~~2~~~~~~~0~~~~~~~0~~~~~~~
00000030: 0000 0033 0000 5e74 0000 0030 0000 0038  3~~~~~~~年~~~~~~0~~~~~~~8~~~~~~~
00000040: 0000 6708 0000 0032 0000 0037 0000 65e5  月~~~~~~2~~~~~~~7~~~~~~~日~~~~~~
""".replace("\r\n", "\n")
    assertEquals(expected, "\n" + dumpString + "\n")
  }

  @Test def testDumpHexAndText2WithIndicator1(): Unit = {

    val bytes = "Date 年月日=2003年08月27日".getBytes("utf-32BE")
    val lengthInBits = bytes.length * 8
    val bb = ByteBuffer.wrap(bytes)

    val dumpString = Dump
      .dump(
        Dump.MixedHexLTR(Some("utf-32BE")),
        0,
        lengthInBits,
        bb,
        includeHeadingLine = true,
        indicatorInfo = Some((0, lengthInBits))
      )
      .mkString("\n")
    val expected = """
          ├─────────────────────────────────────═  ├──────────────────────────────═
87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0~1~2~3~4~5~6~7~8~9~a~b~c~d~e~f~
00000000: 0000 0044 0000 0061 0000 0074 0000 0065  D~~~~~~~a~~~~~~~t~~~~~~~e~~~~~~~
00000010: 0000 0020 0000 5e74 0000 6708 0000 65e5  ␣~~~~~~~年~~~~~~月~~~~~~日~~~~~~
00000020: 0000 003d 0000 0032 0000 0030 0000 0030  =~~~~~~~2~~~~~~~0~~~~~~~0~~~~~~~
00000030: 0000 0033 0000 5e74 0000 0030 0000 0038  3~~~~~~~年~~~~~~0~~~~~~~8~~~~~~~
00000040: 0000 6708 0000 0032 0000 0037 0000 65e5  月~~~~~~2~~~~~~~7~~~~~~~日~~~~~~
""".replace("\r\n", "\n")
    assertEquals(expected, "\n" + dumpString + "\n")
  }

  @Test def testDumpHexAndText2WithIndicator2(): Unit = {

    val bytes = "Date 年月日=2003年08月27日".getBytes("utf-32BE")
    val lengthInBits = bytes.length * 8
    val bb = ByteBuffer.wrap(bytes)

    val dumpString = Dump
      .dump(
        Dump.MixedHexLTR(Some("utf-32BE")),
        0,
        lengthInBits,
        bb,
        includeHeadingLine = true,
        indicatorInfo = Some((16, 12 * 8))
      )
      .mkString("\n")
    val expected = """
               ├───────────────────────────┤           ├──────────────────────┤
87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0~1~2~3~4~5~6~7~8~9~a~b~c~d~e~f~
00000000: 0000 0044 0000 0061 0000 0074 0000 0065  D~~~~~~~a~~~~~~~t~~~~~~~e~~~~~~~
00000010: 0000 0020 0000 5e74 0000 6708 0000 65e5  ␣~~~~~~~年~~~~~~月~~~~~~日~~~~~~
00000020: 0000 003d 0000 0032 0000 0030 0000 0030  =~~~~~~~2~~~~~~~0~~~~~~~0~~~~~~~
00000030: 0000 0033 0000 5e74 0000 0030 0000 0038  3~~~~~~~年~~~~~~0~~~~~~~8~~~~~~~
00000040: 0000 6708 0000 0032 0000 0037 0000 65e5  月~~~~~~2~~~~~~~7~~~~~~~日~~~~~~
""".replace("\r\n", "\n")
    assertEquals(expected, "\n" + dumpString + "\n")
  }

  @Test def testDumpHexAndText3(): Unit = {

    val bytes = "Date 年月日=2003年08月27日".getBytes("utf-16LE")
    val lengthInBits = bytes.length * 8
    val bb = ByteBuffer.wrap(bytes)

    val dumpString = Dump
      .dump(Dump.MixedHexLTR(Some("utf-16LE")), 0, lengthInBits, bb, includeHeadingLine = true)
      .mkString("\n")
    val expected = """
87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0~1~2~3~4~5~6~7~8~9~a~b~c~d~e~f~
00000000: 4400 6100 7400 6500 2000 745e 0867 e565  D~~~a~~~t~~~e~~~␣~~~年~~月~~日~~
00000010: 3d00 3200 3000 3000 3300 745e 3000 3800  =~~~2~~~0~~~0~~~3~~~年~~0~~~8~~~
00000020: 0867 3200 3700 e565                      月~~2~~~7~~~日~~                
""".replace("\r\n", "\n")
    assertEquals(expected, "\n" + dumpString + "\n")
  }

  @Test def testDumpHexAndText4(): Unit = {

    val bytes =
      """da8b f090 a487 f48b be8b be7a 1234 4567 f48b 8018 0156
dada 0000 0101 0817 ece2 8017 ece2 dead beef cc7a 1234
4567 f48b"""
        .replaceAll("\\s+", "")
        .grouped(2)
        .map { Integer.parseInt(_, 16).toByte }
        .toArray
    val lengthInBits = bytes.length * 8
    val bb = ByteBuffer.wrap(bytes)
    val dumpString = Dump
      .dump(Dump.MixedHexLTR(Some("utf-8")), 0, lengthInBits, bb, includeHeadingLine = true)
      .mkString("\n")
    val u068b = Character.toChars(0x068b).mkString
    val u10907 = Character.toChars(0x10907).mkString
    val u10bf8b = Character.toChars(0x10bf8b).mkString
    val u07ad = Character.toChars(0x07ad).mkString
    val u0020 = Character.toChars(0x0020).mkString
    val expected = s"""
87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0~1~2~3~4~5~6~7~8~9~a~b~c~d~e~f~
00000000: da8b f090 a487 f48b be8b be7a 1234 4567  ${u068b}~~~${u10907}~~~~~~~${u10bf8b}~~~~~~~�~z~␒~4~E~g~
00000010: f48b 8018 0156 dada 0000 0101 0817 ece2  �~~~~~␘~␁~V~�~�~␀~␀~␁~␁~␈~␗~�~�~
00000020: 8017 ece2 dead beef cc7a 1234 4567 f48b  ~~␗~�~�~${u0020}${u07ad}${u0020}~~�~�~�~z~␒~4~E~g~�~~~
""".replace("\r\n", "\n")
    assertEquals(expected, "\n" + dumpString + "\n")
  }

  @Test def testDump1(): Unit = {

    val bb = ByteBuffer.wrap((0 to 255).map { _.toByte }.toArray)

    val dumpString = Dump
      .dump(Dump.MixedHexLTR(None), 0, 256 * 8, bb, includeHeadingLine = true)
      .mkString("\n")
    val expected = """
87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0123456789abcdef
00000000: 0001 0203 0405 0607 0809 0a0b 0c0d 0e0f  ␀␁␂␃␄␅␆␇␈␉␊␋␌␍␎␏
00000010: 1011 1213 1415 1617 1819 1a1b 1c1d 1e1f  ␐␑␒␓␔␕␖␗␘␙␚␛␜␝␞␟
00000020: 2021 2223 2425 2627 2829 2a2b 2c2d 2e2f  ␣!"#$%&'()*+,-./
00000030: 3031 3233 3435 3637 3839 3a3b 3c3d 3e3f  0123456789:;<=>?
00000040: 4041 4243 4445 4647 4849 4a4b 4c4d 4e4f  @ABCDEFGHIJKLMNO
00000050: 5051 5253 5455 5657 5859 5a5b 5c5d 5e5f  PQRSTUVWXYZ[\]^_
00000060: 6061 6263 6465 6667 6869 6a6b 6c6d 6e6f  `abcdefghijklmno
00000070: 7071 7273 7475 7677 7879 7a7b 7c7d 7e7f  pqrstuvwxyz{|}~␡
00000080: 8081 8283 8485 8687 8889 8a8b 8c8d 8e8f  €Ɓ‚ƒ„…†‡ˆ‰Š‹ŒƍŽƏ
00000090: 9091 9293 9495 9697 9899 9a9b 9c9d 9e9f  Ɛ‘’“”•–—˜™š›œƝžŸ
000000a0: a0a1 a2a3 a4a5 a6a7 a8a9 aaab acad aeaf  ␢¡¢£¤¥¦§¨©ª«¬-®¯
000000b0: b0b1 b2b3 b4b5 b6b7 b8b9 babb bcbd bebf  °±²³´µ¶·¸¹º»¼½¾¿
000000c0: c0c1 c2c3 c4c5 c6c7 c8c9 cacb cccd cecf  ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏ
000000d0: d0d1 d2d3 d4d5 d6d7 d8d9 dadb dcdd dedf  ÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞß
000000e0: e0e1 e2e3 e4e5 e6e7 e8e9 eaeb eced eeef  àáâãäåæçèéêëìíîï
000000f0: f0f1 f2f3 f4f5 f6f7 f8f9 fafb fcfd feff  ðñòóôõö÷øùúûüýþÿ
""".replace("\r\n", "\n")
    assertEquals(expected, "\n" + dumpString + "\n")
  }

  @Test def testDump2(): Unit = {

    val bb = ByteBuffer.wrap((0 to 255).map { _.toByte }.toArray)

    val dumpString = Dump
      .dump(Dump.MixedHexLTR(None), 8, 254 * 8, bb, includeHeadingLine = true)
      .mkString("\n")

    val expected = """
87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0123456789abcdef
00000000:   00 0102 0304 0506 0708 090a 0b0c 0d0e   ␀␁␂␃␄␅␆␇␈␉␊␋␌␍␎
00000010: 0f10 1112 1314 1516 1718 191a 1b1c 1d1e  ␏␐␑␒␓␔␕␖␗␘␙␚␛␜␝␞
00000020: 1f20 2122 2324 2526 2728 292a 2b2c 2d2e  ␟␣!"#$%&'()*+,-.
00000030: 2f30 3132 3334 3536 3738 393a 3b3c 3d3e  /0123456789:;<=>
00000040: 3f40 4142 4344 4546 4748 494a 4b4c 4d4e  ?@ABCDEFGHIJKLMN
00000050: 4f50 5152 5354 5556 5758 595a 5b5c 5d5e  OPQRSTUVWXYZ[\]^
00000060: 5f60 6162 6364 6566 6768 696a 6b6c 6d6e  _`abcdefghijklmn
00000070: 6f70 7172 7374 7576 7778 797a 7b7c 7d7e  opqrstuvwxyz{|}~
00000080: 7f80 8182 8384 8586 8788 898a 8b8c 8d8e  ␡€Ɓ‚ƒ„…†‡ˆ‰Š‹ŒƍŽ
00000090: 8f90 9192 9394 9596 9798 999a 9b9c 9d9e  ƏƐ‘’“”•–—˜™š›œƝž
000000a0: 9fa0 a1a2 a3a4 a5a6 a7a8 a9aa abac adae  Ÿ␢¡¢£¤¥¦§¨©ª«¬-®
000000b0: afb0 b1b2 b3b4 b5b6 b7b8 b9ba bbbc bdbe  ¯°±²³´µ¶·¸¹º»¼½¾
000000c0: bfc0 c1c2 c3c4 c5c6 c7c8 c9ca cbcc cdce  ¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎ
000000d0: cfd0 d1d2 d3d4 d5d6 d7d8 d9da dbdc ddde  ÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞ
000000e0: dfe0 e1e2 e3e4 e5e6 e7e8 e9ea ebec edee  ßàáâãäåæçèéêëìíî
000000f0: eff0 f1f2 f3f4 f5f6 f7f8 f9fa fbfc fd    ïðñòóôõö÷øùúûüý 
""".replace("\r\n", "\n")

    assertEquals(expected, "\n" + dumpString + "\n")
  }

  @Test def testDump3(): Unit = {

    val bb = ByteBuffer.wrap((0 to 255).map { _.toByte }.toArray)

    val dumpString = Dump
      .dump(Dump.MixedHexLTR(None), 50, (191 * 8) - 5, bb, includeHeadingLine = true)
      .mkString("\n")
    val expected = """
87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0123456789abcdef
00000000:                0001 0203 0405 0607 0809        ␀␁␂␃␄␅␆␇␈␉
00000010: 0a0b 0c0d 0e0f 1011 1213 1415 1617 1819  ␊␋␌␍␎␏␐␑␒␓␔␕␖␗␘␙
00000020: 1a1b 1c1d 1e1f 2021 2223 2425 2627 2829  ␚␛␜␝␞␟␣!"#$%&'()
00000030: 2a2b 2c2d 2e2f 3031 3233 3435 3637 3839  *+,-./0123456789
00000040: 3a3b 3c3d 3e3f 4041 4243 4445 4647 4849  :;<=>?@ABCDEFGHI
00000050: 4a4b 4c4d 4e4f 5051 5253 5455 5657 5859  JKLMNOPQRSTUVWXY
00000060: 5a5b 5c5d 5e5f 6061 6263 6465 6667 6869  Z[\]^_`abcdefghi
00000070: 6a6b 6c6d 6e6f 7071 7273 7475 7677 7879  jklmnopqrstuvwxy
00000080: 7a7b 7c7d 7e7f 8081 8283 8485 8687 8889  z{|}~␡€Ɓ‚ƒ„…†‡ˆ‰
00000090: 8a8b 8c8d 8e8f 9091 9293 9495 9697 9899  Š‹ŒƍŽƏƐ‘’“”•–—˜™
000000a0: 9a9b 9c9d 9e9f a0a1 a2a3 a4a5 a6a7 a8a9  š›œƝžŸ␢¡¢£¤¥¦§¨©
000000b0: aaab acad aeaf b0b1 b2b3 b4b5 b6b7 b8b9  ª«¬-®¯°±²³´µ¶·¸¹
000000c0: babb bcbd bebf                           º»¼½¾¿          
""".replace("\r\n", "\n")

    assertEquals(expected, "\n" + dumpString + "\n")
  }

  @Test def testDump4(): Unit = {

    val bb = ByteBuffer.wrap((0 to 255).map { _.toByte }.toArray)

    val dumpString =
      Dump.dump(Dump.MixedHexLTR(None), 50, 51, bb, includeHeadingLine = true).mkString("\n")
    val expected = """
87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0123456789abcdef
00000000:                0001 0203 0405 0607             ␀␁␂␃␄␅␆␇  
""".replace("\r\n", "\n")
    assertEquals(expected, "\n" + dumpString + "\n")
  }

  @Test def testDumpLSBFirst1(): Unit = {

    val bb = ByteBuffer.wrap((0 to 255).map { _.toByte }.toArray)

    val dumpString =
      Dump.dump(Dump.MixedHexRTL(None), 50, 51, bb, includeHeadingLine = true).mkString("\n")
    val expected = """
fedcba9876543210  ffee ddcc bbaa 9988 7766 5544 3322 1100  87654321
  ␇␆␅␄␃␂␁␀             0706 0504 0302 0100                :00000000
""".replace("\r\n", "\n")
    assertEquals(expected, "\n" + dumpString + "\n")
  }

  @Test def testDumpLSBFirst2(): Unit = {

    val bytes: Array[Byte] =
      """E4 67 00 80 55
        67 92 1A FC 77 00
        00 00 80 F8 63 44 48 10 FE B6 9B 01 00 00"""
        .split("\\s+")
        .map { Integer.parseInt(_, 16).toByte }
        .toArray
    val bb = ByteBuffer.wrap(bytes)

    val dumpString = Dump
      .dump(Dump.MixedHexRTL(None), 0, bytes.length * 8, bb, includeHeadingLine = true)
      .mkString("\n")
    val expected = """
fedcba9876543210  ffee ddcc bbaa 9988 7766 5544 3322 1100  87654321
cø€␀␀␀wü␚’gU€␀gä  63f8 8000 0000 77fc 1a92 6755 8000 67e4 :00000000
       ␀␀␁›¶þ␐HD                   00 0001 9bb6 fe10 4844 :00000010
""".replace("\r\n", "\n")
    assertEquals(expected, "\n" + dumpString + "\n")
  }

  @Test def testDumpTextLine1(): Unit = {
    val data = (0 to 255).map { _.toByte }.toArray
    val bb = ByteBuffer.wrap(data)
    val lengthInbits = data.length * 8
    val dump = Dump.dump(Dump.TextOnly(None), 0, lengthInbits, bb)
    val dumpString = dump.mkString("\n")
    val expected = """␀␁␂␃␄␅␆␇␈␉␊␋␌␍␎␏␐␑␒␓␔␕␖␗␘␙␚␛␜␝␞␟␣!"#$%&'()*+,-./0123456789:;<=>?@ABCDE"""
    // """FGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~␡€Ɓ‚ƒ„…†‡ˆ‰Š‹ŒƍŽƏƐ‘’“”•–—˜™š›œƝžŸ␢¡¢£¤¥¦§¨©ª«¬-®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ""".replace("\r\n", "\n")
    assertEquals(expected, dumpString)
  }

  @Test def testDumpTextLine2(): Unit = {
    val data = (0 to 255).map { _.toByte }.toArray
    val bb = ByteBuffer.wrap(data)
    val lengthInbits = data.length * 8
    val indicatorStartAtByte0b = 32L
    val indicatorLengthInBytes = 8
    val dump = Dump.dump(
      Dump.TextOnly(None),
      0,
      lengthInbits,
      bb,
      indicatorInfo = Some((indicatorStartAtByte0b * 8, indicatorLengthInBytes * 8))
    )
    val dumpString = dump.mkString("\n")
    val expected =
      """|                                ├──────┤
         |␀␁␂␃␄␅␆␇␈␉␊␋␌␍␎␏␐␑␒␓␔␕␖␗␘␙␚␛␜␝␞␟␣!"#$%&'()*+,-./0123456789:;<=>?@ABCDE""".stripMargin
        .replace("\r\n", "\n")
    // """FGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~␡€Ɓ‚ƒ„…†‡ˆ‰Š‹ŒƍŽƏƐ‘’“”•–—˜™š›œƝžŸ␢¡¢£¤¥¦§¨©ª«¬-®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ""".replace("\r\n", "\n")
    assertEquals(expected, dumpString)
  }

  @Test def testDumpTextLine3(): Unit = {
    val data = (32 to 255).map { _.toByte }.toArray
    val bb = ByteBuffer.wrap(data)
    val lengthInbits = data.length * 8
    val indicatorStartAtByte0b = 0L
    val indicatorLengthInBytes = 1000
    val dump = Dump.dump(
      Dump.TextOnly(None),
      0,
      lengthInbits,
      bb,
      indicatorInfo = Some((indicatorStartAtByte0b * 8, indicatorLengthInBytes * 8))
    )
    val dumpString = dump.mkString("\n")

    //
    // Careful below.
    // Eclipse seems to want to replace the consecutive dashes with
    // narrower characters, but seemingly only if it sees the .stripMargin('#') on the same line
    // after the rich string. WTF?
    // Anyway, splitting it and putting stripMargin on the separate line seems
    // to defeat this 'feature' of Eclipse.
    //
    val ex =
      """#├────────────────────────────────────────────────────────────────────═
         #␣!"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcde"""
    val expected = ex.stripMargin('#').replace("\r\n", "\n")

    // """FGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~␡€Ɓ‚ƒ„…†‡ˆ‰Š‹ŒƍŽƏƐ‘’“”•–—˜™š›œƝžŸ␢¡¢£¤¥¦§¨©ª«¬-®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ""".replace("\r\n", "\n")
    assertEquals(expected, dumpString)
  }

  @Test def testDumpTextLine4(): Unit = {
    val data = (32 to 63).map { _.toByte }.toArray
    val bb = ByteBuffer.wrap(data)
    val lengthInbits = data.length * 8
    val indicatorStartAtByte0b = 0L
    val indicatorLengthInBytes = 1000
    val dump = Dump.dump(
      Dump.TextOnly(None),
      0,
      lengthInbits,
      bb,
      indicatorInfo = Some((indicatorStartAtByte0b * 8, indicatorLengthInBytes * 8))
    )
    val dumpString = dump.mkString("\n")
    //
    // Careful below.
    // Eclipse seems to want to replace the consecutive dashes with
    // narrower characters, but seemingly only if it sees the .stripMargin('#') on the same line
    // after the rich string. WTF?
    // Anyway, splitting it and putting stripMargin on the separate line seems
    // to defeat this 'feature' of Eclipse.
    //
    val ex =
      """#├──────────────────────────────┤
         #␣!"#$%&'()*+,-./0123456789:;<=>?"""
    val expected = ex.stripMargin('#').replace("\r\n", "\n")
    assertEquals(expected, dumpString)
  }

  @Test def testDumpTextLine5(): Unit = {
    val data = (32 to 63).map { _.toByte }.toArray
    val bb = ByteBuffer.wrap(data)
    val lengthInbits = data.length * 8
    val indicatorStartAtByte0b = 5L
    val indicatorLengthInBytes = 0
    val dump = Dump.dump(
      Dump.TextOnly(None),
      0,
      lengthInbits,
      bb,
      indicatorInfo = Some((indicatorStartAtByte0b * 8, indicatorLengthInBytes * 8))
    )
    val dumpString = dump.mkString("\n")
    //
    // Careful below.
    // That first line of the expected ends in a vertical box drawing character
    // which is NOT the same as the pipe or | on the keyboard.
    //
    val expected =
      """#     │
         #␣!"#$%&'()*+,-./0123456789:;<=>?""".stripMargin('#').replace("\r\n", "\n")
    assertEquals(expected, dumpString)
  }

  @Test def testDumpTextLine6(): Unit = {
    val data = (32 to 63).map { _.toByte }.toArray
    val bb = ByteBuffer.wrap(data)
    val lengthInbits = data.length * 8
    val indicatorStartAtByte0b = 5L
    val indicatorLengthInBytes = 1
    val dump = Dump.dump(
      Dump.TextOnly(None),
      0,
      lengthInbits,
      bb,
      indicatorInfo = Some((indicatorStartAtByte0b * 8, indicatorLengthInBytes * 8))
    )
    val dumpString = dump.mkString("\n")
    val expected =
      """#     ║
         #␣!"#$%&'()*+,-./0123456789:;<=>?""".stripMargin('#').replace("\r\n", "\n")
    assertEquals(expected, dumpString)
  }

  @Test def testDumpTextLine7(): Unit = {
    val data = """da8b f090 a487 f48b be8b be7a 1234
      4567 f48b 8018 0156 dada
      0000 0101 0817 dead beef cc7a"""
      .replaceAll("\\s+", "")
      .grouped(2)
      .map { Integer.parseInt(_, 16).toByte }
      .toArray
    val bb = ByteBuffer.wrap(data)
    val lengthInbits = data.length * 8
    val dumpString = Dump.dump(Dump.TextOnly(Some("utf-8")), 0, lengthInbits, bb).mkString("\n")
    val uUnknown = 0xfffd
    val arrayOfDecodedChars1 =
      Array(0x068b, 0x10907, 0x10bf8b, uUnknown, 0x007a)
    val arrayOfDecodedChars2 = Array(0x0034, 0x0045, 0x0067, uUnknown)
    val arrayOfDecodedChars3 = Array(0x0056, uUnknown, uUnknown)
    val arrayOfDecodedChars4 =
      Array(0x0020, 0x07ad, 0x0020, uUnknown, uUnknown, uUnknown, 0x007a)
    val expected =
      s"""${arrayOfDecodedChars1.map(Character.toChars(_).mkString).mkString}␒""" +
        s"""${arrayOfDecodedChars2.map(Character.toChars(_).mkString).mkString}␘␁""" +
        s"""${arrayOfDecodedChars3.map(Character.toChars(_).mkString).mkString}␀␀␁␁␈␗""" +
        s"""${arrayOfDecodedChars4.map(Character.toChars(_).mkString).mkString}"""
          .replace("\r\n", "\n")
    assertEquals(expected, dumpString)
  }

}
