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