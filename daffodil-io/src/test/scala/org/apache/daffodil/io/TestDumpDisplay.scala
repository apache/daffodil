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

import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Assert._
import org.junit.Test

class TestDumpDisplay {
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val tdml = XMLUtils.TDML_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val tns = XMLUtils.EXAMPLE_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE

  /**
   * This test shows that any byte becomes a character with a glyph.
   *
   * This is used when creating readable dumps of data that might
   * actually not be text at all, when we don't know the encoding.
   */
  @Test def testAllPrintableChars() = {

    val bytes: Array[Byte] = (0 to 255).map { _.toByte }.toArray
    val res = Misc.remapBytesToStringOfVisibleGlyphs(bytes)
    val exp =
      //
      // C0 Controls - use unicode control picture characters.
      "␀␁␂␃␄␅␆␇␈␉␊␋␌␍␎␏␐␑␒␓␔␕␖␗␘␙␚␛␜␝␞␟" +
        "␣" + // space
        """!"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~""" + // ascii glyph
        "\u2421" + // DEL
        //
        // C1 controls, which windows 1252 mostly has glyphs for
        // These are the equivalent unicode characters. The ones that windows-1252 has no glyph or are whitespace
        // are remapped to other characters noted
        "€" +
        "Ɓ" + // 0x81 mapped to Ɓ
        "‚ƒ„…†‡ˆ‰Š‹Œ" +
        "ƍ" + // 0x8D mapped to ƍ
        "Ž" +
        "Ə" + // 0x8F mapped to Ə
        "Ɛ" + // 0x90 mapped to Ɛ
        """‘’“”•–—˜™š›œ""" +
        "Ɲ" + // 0x9D mapped to Ɲ
        "žŸ" +
        """␢""" + // 0xA0 non-break space mapped to small b with stroke
        "¡¢£¤¥¦§¨©ª«¬" +
        "-" + // 0xAD soft hyphen mapped to regular hyphen (because soft hyphen seems to be a zero-width in many fonts.
        "®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"
    assertEquals(exp.length, res.length)
    val sb = new StringBuilder()
    ((exp.zip(res)).zip(0 to res.length)).foreach { case ((exp, res), i) =>
      if (exp != res) {
        sb.append(s"At index $i expected '$exp' but actual was '$res'\n")
      }
    }
    val msg = sb.toString()
    if (msg.nonEmpty) fail(msg)
  }

}
