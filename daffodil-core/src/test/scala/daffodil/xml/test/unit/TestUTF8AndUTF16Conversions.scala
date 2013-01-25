package daffodil.xml.test.unit

import scala.xml._
import junit.framework.Assert._
import org.scalatest.junit.JUnitSuite
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.util.Arrays;
import org.junit.Test

/**
 * These tests characterize utf-8 and utf-16 character set conversion behavior
 * so that the assumptions are clear that we depend on in our code. E.g., that Byte-order-marks
 * don't cause anything to blow up, and that surrogate pairs really do work, etc.
 *
 * These things are some of the corner cases in the world of character set conversions, and
 * knowing they work appropriately lets our code be less conservative. E.g., we don't put checks
 * for various corner cases all over our code if we can show it will work right here.
 *
 * Really great converter site: http://rishida.net/tools/conversion/
 */

class TestUTF8AndUTF16Conversions extends JUnitSuite {

  @Test def testDecoding() {
    // This is dollar sign, cents sign, euro symbol, 
    // and a 3-byte unicode whitespace character (which has been problematic
    // some places we think)
    val data = <data>&#x24;&#xA2;&#x20AC;&#x2028;</data>
    val str = data.child.text
    assertEquals(4, str.length)
    val bytes = str.getBytes("utf-8")
    assertEquals(9, bytes.length)
  }

  @Test def testEquality() {
    val data = <data><d>&#x24;</d></data>
    val data2 = <data><d>$</d></data>
    assertEquals(data2, data)
  }

  @Test def testSupplementalCharSurrogates() {
    // U+1d420 is a script capital A which is part of the unicode supplemental characters
    // which requires a surrogate pair in the JVM/Java/Scala, and 4 bytes of UTF-8 representation.
    val data = <data>&#x1d420;</data>
    val str = data.child.text
    //    println("%x".format(str(0).toInt))
    //    println("%x".format(str(1).toInt))
    assertEquals(2, str.length) // 2 because that U+1d420 requires two utf-16 codepoints to represent it.
    val bytes = str.getBytes("utf-8")
    assertEquals(4, bytes.length) // encodes to 4 bytes.

  }

  /**
   * Tests that our utf-8 converting can tolerate the old style
   * where surrogate-pair characters were represented by
   * representing each surrogate code point separately in a 3-byte encoding
   * (total of 6 bytes for the character) instead of the more modern
   * (and required!) 4-byte utf-8 encoding.
   */
  @Test def testReadSurrogateCodePointsLikeCharacters() {

    val data = <data>&#xd835;&#xdcd0;</data> //technically, this is illegal.
    val str = data.child.text
    assertEquals(2, str.length)
    val d835 = str(0).toInt
    val dcd0 = str(1).toInt
    val bytes = str.getBytes("utf-8")
    assertEquals(4, bytes.length) // If you convert back to utf-8, you get a 4-byte encoding

    val alternateOldRep: Array[Byte] = Array(
      // Compatibility with pre-surrogate world.
      // Same U+1d4d0, but represented as a surrogate pair, each surrogate then
      // represented as a 3-byte UTF-8 sequence. (This is an older technique).
      0xED.toByte, 0xA0.toByte, 0xB5.toByte, 0xED.toByte, 0xB3.toByte, 0x90.toByte)
    val decodedAlt = new String(alternateOldRep, "utf-8")
    assertEquals(2, decodedAlt.length)
    assertEquals(d835, decodedAlt(0).toInt)
    assertEquals(dcd0, decodedAlt(1).toInt)

  }

}