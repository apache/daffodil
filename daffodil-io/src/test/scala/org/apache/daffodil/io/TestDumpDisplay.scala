/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.io

import org.junit.Test
import org.junit.Assert._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import java.nio.ByteBuffer
import java.nio.CharBuffer
import edu.illinois.ncsa.daffodil.util.Misc

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

    // val bytes = 0 to 255 map { _.toByte }
    val bb = ByteBuffer.allocate(256)
    0 to 255 foreach { n => bb.put(n, n.toByte) }
    val cb = CharBuffer.allocate(256)
    Misc.remapBytesToVisibleGlyphs(bb, cb)
    val res = cb.toString
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
    assertEquals(exp, res)
  }

}