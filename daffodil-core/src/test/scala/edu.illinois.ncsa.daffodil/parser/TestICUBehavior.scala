package edu.illinois.ncsa.daffodil.parser

import scala.xml._
import junit.framework.Assert._
import org.scalatest.junit.JUnitSuite
import com.ibm.icu._
import java.io._
import java.nio._
import java.nio.charset._
import java.util.Iterator
import com.ibm.icu.charset.CharsetCallback._
import com.ibm.icu.charset._
import Converter._ // Need implicit converter
import org.junit.Test

/**
 * These tests characterize behavior of the ICU library. We're going to have to rely on
 * some detailed behaviors here, so the point of these tests is to isolate behaviors we
 * depend on. If these tests break, then there's a high likelihood that something in ICU
 * changed in a way that will break daffodil.
 *
 * In particular, the error handling behavior of ICU. When it calls the error hooks, and
 * with exactly what information, is something we're going to have to be well wired into.
 */

/**
 * We're going to make use of the Unicode character U+1d4d0, which is a Mathematical Script A.
 * It requires a surrogate pair in UTF-16 which is: D835 DCD0
 * It encodes to 4 bytes in UTF-8 of: F0 9D 93 90
 *
 * We also make use of U+11000 (that's hex). This is 1 past the end of the legal Unicode character range
 * which the max code point is U+10FFFF.
 *
 * If you use the UTF-16 surrogate pair "scheme", the bits will fit, and you'll get
 * this surrogate pair: D804 DC00 (I think)
 *
 * If you encode into UTF-8, it fits in 4 bytes as these bytes: F0 91 80 80
 *
 * Also, there is 0x1FFFFF - this is the highest hex value that the UTF-8 scheme can encode in 4 bytes.
 */

/**
 * How to write one of these handlers is a bit of a black art.
 * The only examples I could find  are the ones that are part of ICU itself.
 * They are named CharsetCallback.Decoder.To_U_CALLBACK_STOP, TO_U_CALLBACK_ESCAPE,
 * TO_U_CALLBACK_SUBSTITUTE, and TO_U_CALLBACK_SKIP.
 *
 * These handlers are the core of implementing DFDL features to support a recently
 * proposed feature of dfdl:encodingErrorPolicy='skip' (or 'replace' or 'error'). This
 * is also likely a place for some Daffodil specific features. For example, I think
 * some people may want illegal bytes preserved by mapping them into the Unicode PrivateUseArea
 * bytes 00 to FF become E000 to E0FF (dfdlx:encodingErrorReplacement="useUnicodePrivateUseArea")
 * These print nicely in a way that makes the byte content visible.
 * Another possibility is to convert illegal bytes into %XX expansions, or XML style &#xFF; hex
 * numeric entities.
 */
class DecodeMalformedHandler extends CharsetCallback.Decoder {
  def call(decoder: CharsetDecoderICU,
           context: Any,
           source: ByteBuffer,
           target: CharBuffer,
           offsets: IntBuffer,
           buffer: Array[Char],
           length: Int,
           cr: CoderResult): CoderResult = {
    // println("Decode: " + cr.toString)
    hook()
    cr.throwException()
    cr
  }

  def hook() {
    // override to do something like count
  }
}

class DecodeUnmappableHandler extends CharsetCallback.Decoder {
  def call(decoder: CharsetDecoderICU,
           context: Any,
           source: ByteBuffer,
           target: CharBuffer,
           offsets: IntBuffer,
           buffer: Array[Char],
           length: Int,
           cr: CoderResult): CoderResult = {
    // println("Decode: " + cr.toString)
    hook()
    cr.throwException()
    cr
  }

  def hook() {}
}

class EncodeMalformedHandler extends CharsetCallback.Encoder {
  def call(encoder: CharsetEncoderICU,
           context: Any,
           source: CharBuffer,
           target: ByteBuffer,
           offsets: IntBuffer,
           buffer: Array[Char],
           length: Int,
           cp: Int,
           cr: CoderResult): CoderResult = {
    // println("Encode: " + cr.toString)
    hook()
    cr.throwException()
    cr
  }

  def hook() {}
}

class EncodeUnmappableHandler extends CharsetCallback.Encoder {
  def call(encoder: CharsetEncoderICU,
           context: Any,
           source: CharBuffer,
           target: ByteBuffer,
           offsets: IntBuffer,
           buffer: Array[Char],
           length: Int,
           cp: Int,
           cr: CoderResult): CoderResult = {
    // println("Encode: " + cr.toString)
    hook()
    cr.throwException()
    cr
  }

  def hook() {}
}

object Converter {
  def convert(in: InputStream, out: OutputStream, inDecoder: CharsetDecoder, outEncoder: CharsetEncoder) = {
    val i = new BufferedReader(new InputStreamReader(in, inDecoder));
    val o = new BufferedWriter(new OutputStreamWriter(out, outEncoder));
    val cb = CharBuffer.allocate(65536);

    var nCharsRead = 0;
    nCharsRead = i.read(cb);
    while (nCharsRead > 0) {
      cb.flip();
      o.append(cb);
      cb.clear();
      nCharsRead = i.read(cb);
    }
    o.flush();
  }

  def parse(in: InputStream, inDecoder: CharsetDecoder): String = {
    val r = new BufferedReader((new InputStreamReader(in, inDecoder)))
    val s = r.readLine()
    s
  }

  def unparse(out: OutputStream, outEncoder: CharsetEncoder)(s: String) = {
    val o = new BufferedWriter(new OutputStreamWriter(out, outEncoder));
    o.append(s);
    o.flush();
  }

  implicit def intArrayToByteArray(intArray: Array[Int]): Array[Byte] = {
    intArray.map(int => int.asInstanceOf[Byte]).toArray
  }
}

class TestUnicodeICUErrorTolerance extends JUnitSuite {

  @Test def testIntArrayToByteArray() {
    val ia = Array[Int](1, 127, 128, 255, 0)
    val actualByteArray = Converter.intArrayToByteArray(ia)
    val expectedByteArray = Array[Byte](1, 127, -128, -1, 0)
    val pairs = expectedByteArray zip actualByteArray
    for ((exp, act) <- pairs) {
      assertEquals(exp, act)
    }
  }

  /**
   * Scala, like Java, tolerates isolated broken surrogate halves.
   */
  @Test def testScalaAllowsBadUnicode() {
    val exp = "@@@\udcd0@@@" // that's the 2nd half of a surrogate pair for U+1d4d0 sandwiched between @@@ 
    val codepoint = exp.charAt(3)
    assertEquals(0xdcd0, codepoint)
  }

  /**
   * This test shows that ICU isn't tolerating 3-byte encodings of surrogates if they are
   * isolated.
   */
  @Test def testUTF8Decode3ByteSurrogateIsMalformed() {
    val exp = "\udcd0" // that's the trailing surrogate in the surrogate pair for U+1d4d0
    val cs = CharsetICU.forNameICU("utf-8")
    val dn = cs.displayName()
    assertEquals("UTF-8", dn)
    val decoder = cs.newDecoder().asInstanceOf[CharsetDecoderICU]
    //
    // When you setup these handlers, they seem to get called regardless of
    // what the first argument is. E.g., the hook gets called for length 3 malformed as well as length 1 malformed.
    //
    decoder.setToUCallback(CoderResult.malformedForLength(1), new DecodeMalformedHandler(), this)
    decoder.setToUCallback(CoderResult.unmappableForLength(1), new DecodeUnmappableHandler(), this)

    val inBuf = Array[Int]( // 3 byte encoding of 2nd half of surrogate pair for U+1d4d0                   
      0xED, 0xB3, 0x90)
    val input = new ByteArrayInputStream(inBuf);

    val exc = intercept[MalformedInputException] {
      val act = Converter.parse(input, decoder)
    }
    val badLength = exc.getInputLength()
    assertEquals(3, badLength)
  }

  /**
   * This test shows that ICU isn't tolerating 3-byte encodings of surrogates if they are
   * isolated. It is substituting for them.
   */
  @Test def testUTF8Decode3ByteSurrogateReplacement() {
    val inBuf = Array[Int](0xED, 0xB3, 0x90)
    val act = replaceBadCharacters(inBuf)
    assertEquals("\uFFFD", act)
  }

  /**
   * This test shows that ICU substitutes on encoding/output also when an isolated surrogate
   * is presented to the encoder.
   */
  @Test def testUTF8Encode3ByteSurrogateReplacement() {
    val s = "\ud800"
    val act = replaceBadCharactersEncoding(s)
    val exp = Array[Int](0xEF, 0xBF, 0xBD) // the 3-byte UTF-8 replacement sequence 
    // which is just the UTF-8 encoding of the Unicode replacement character U+FFFD.
    for ((e, a) <- exp zip act) {
      assertEquals(e, a)
    }
  }

  /**
   * This test shows that ICU isn't tolerating 3-byte encodings of surrogates if they are
   * isolated when encoding into utf-8 from internal Java/Scala strings.
   *
   * also shows that when there are back-to back problems in the String, that the exception
   * is thrown for the very first one.
   *
   * Also, the "length" of the malformed input is 1, as in one character that we're encoding.
   * I.e., not measured in bytes here.
   */
  @Test def testUTF8Encode3ByteSurrogateIsMalformed() {
    val s = "\udcd0\udcd0\udcd0\udcd0" // that's the 2nd half of a surrogate pair for U+1d4d0
    val cs = CharsetICU.forNameICU("utf-8")
    val dn = cs.displayName()
    assertEquals("UTF-8", dn)
    val encoder = cs.newEncoder().asInstanceOf[CharsetEncoderICU]
    //
    // When you setup these handlers, they seem to get called regardless of
    // what the first argument is. E.g., the hook gets called for length 3 malformed as well as length 1 malformed.
    //
    encoder.setFromUCallback(CoderResult.malformedForLength(1), new EncodeMalformedHandler(), this)
    encoder.setFromUCallback(CoderResult.unmappableForLength(1), new EncodeUnmappableHandler(), this)
    val exp: Array[Byte] = Array[Int]( // 3 byte encoding of 2nd half of surrogate pair for U+1d4d0                   
      0xED, 0xB3, 0x90)
    val output = new ByteArrayOutputStream();

    val exc = intercept[MalformedInputException] {
      Converter.unparse(output, encoder)(s)
    }
    assertEquals(1, exc.getInputLength())
  }

  /**
   * shows that the codepoints that require surrogate pairs do in fact create two Java/Scala string codepoints.
   */
  @Test def testUTF8ToSurrogatePair() {
    val exp = "\ud800\udc00" // surrogate pair for U+010000
    val cs = CharsetICU.forNameICU("utf-8")
    val dn = cs.displayName()
    assertEquals("UTF-8", dn)
    val decoder = cs.newDecoder().asInstanceOf[CharsetDecoderICU]
    decoder.setToUCallback(CoderResult.malformedForLength(1), new DecodeMalformedHandler(), this)
    decoder.setToUCallback(CoderResult.unmappableForLength(1), new DecodeUnmappableHandler(), this)
    val inBuf: Array[Byte] = Array[Int](
      // 4 byte encoding of U+010000 (that's hex) which is the first character that requires a surrogate pair.                  
      0xF0, 0x90, 0x80, 0x80)
    val input = new ByteArrayInputStream(inBuf);
    val act = Converter.parse(input, decoder)
    assertEquals(exp, act)
  }

  /**
   * shows that really wild UTF-8 variants that support up to 6 bytes per character
   * aren't fully supported.
   *
   * Of course this is really extreme as there is clearly no surrogate pair represntation
   * of this code point possible.
   */
  @Test def testUTF8Extreme6ByteToSurrogatePair() {
    val cs = CharsetICU.forNameICU("utf-8")
    val dn = cs.displayName()
    assertEquals("UTF-8", dn)
    val decoder = cs.newDecoder().asInstanceOf[CharsetDecoderICU]
    decoder.setToUCallback(CoderResult.malformedForLength(1), new DecodeMalformedHandler(), this)
    decoder.setToUCallback(CoderResult.unmappableForLength(1), new DecodeUnmappableHandler(), this)
    val inBuf: Array[Byte] = Array[Int](
      // 6 byte encoding of \x7FFFFFFF                
      0xFD, 0xBF, 0xBF, 0xBF, 0xBF, 0xBF, 0xBF)
    val input = new ByteArrayInputStream(inBuf);
    val e = intercept[MalformedInputException] {
      Converter.parse(input, decoder)
    }
    assertEquals(6, e.getInputLength())
  }

  @Test def testUTF8Extreme4ByteToSurrogatePair() {
    val cs = CharsetICU.forNameICU("utf-8")
    val dn = cs.displayName()
    assertEquals("UTF-8", dn)
    val decoder = cs.newDecoder().asInstanceOf[CharsetDecoderICU]
    decoder.setToUCallback(CoderResult.malformedForLength(1), new DecodeMalformedHandler(), this)
    decoder.setToUCallback(CoderResult.unmappableForLength(1), new DecodeUnmappableHandler(), this)
    val inBuf: Array[Byte] = Array[Int](
      // 4 byte encoding of \x110000                
      0xF4, 0x90, 0x80, 0x80)
    val input = new ByteArrayInputStream(inBuf);
    val e = intercept[MalformedInputException] { // fails to convert because there is no possible surrogate-pair rep for this.
      Converter.parse(input, decoder)
    }
    assertEquals(4, e.getInputLength())
  }

  /**
   * This test shows that ICU isn't tolerating 3-byte encodings (CESU-8 encoding) of surrogates at all, it's not
   * accepting them if they are properly matched even.
   */
  @Test def testUTF8Decode6ByteSurrogatePairIsMalformed() {
    val exp = "\ud4d0" // that's the 2nd half of a surrogate pair for U+1d4d0
    val cs = CharsetICU.forNameICU("utf-8")
    val dn = cs.displayName()
    assertEquals("UTF-8", dn)
    val decoder = cs.newDecoder().asInstanceOf[CharsetDecoderICU]
    decoder.setToUCallback(CoderResult.malformedForLength(1), new DecodeMalformedHandler(), this)
    decoder.setToUCallback(CoderResult.unmappableForLength(1), new DecodeUnmappableHandler(), this)
    val inBuf = Array[Int](
      // Compatibility with pre-surrogate world.
      // Character U+1d4d0, but represented as a surrogate pair, each surrogate then
      // represented as a 3-byte UTF-8 sequence. (This is an older technique).
      0xED, 0xA0, 0xB5, 0xED, 0xB3, 0x90)
    val input = new ByteArrayInputStream(inBuf);

    val exc = intercept[MalformedInputException] {
      val act = Converter.parse(input, decoder)
    }
    val badLength = exc.getInputLength()
    assertEquals(3, badLength)
  }

  /**
   * This test just confirms this statement (taken from ICU web site)
   *
   * 16-bit Unicode strings in internal processing contain sequences of 16-bit code units that may
   * not always be well-formed UTF-16. ICU treats single, unpaired surrogates as surrogate code points,
   * i.e., they are returned in per-code point iteration, they are included in the number of code points
   * of a string, and they are generally treated much like normal, unassigned code points in most APIs.
   * Surrogate code points have Unicode properties although they cannot be assigned an actual character.
   *
   * ICU string handling functions (including append, substring, etc.) do not automatically protect
   * against producing malformed UTF-16 strings.
   */
  @Test def testUTF16DecodeBadSurrogate() {
    val exp = "\ud4d0" // that's the 2nd half of a surrogate pair for U+1d4d0
    val cs = CharsetICU.forNameICU("utf-16BE")
    val dn = cs.displayName()
    assertEquals("UTF-16BE", dn)
    val decoder = cs.newDecoder().asInstanceOf[CharsetDecoderICU]
    decoder.setToUCallback(CoderResult.malformedForLength(2), new DecodeMalformedHandler(), this)
    decoder.setToUCallback(CoderResult.unmappableForLength(2), new DecodeUnmappableHandler(), this)
    val inBuf = Array[Int](0xd4, 0xd0) // 2nd surrogate in pair for U+1d4d0
    val input = new ByteArrayInputStream(inBuf);
    val act = Converter.parse(input, decoder)
    assertEquals(exp, act)
  }

  /**
   * BOM's in middle of UTF-16 strings cause no problems.
   */
  @Test def testUTF16DecodeBOMsInMidString() {
    val exp = "\uFEFF@\uFEFF@" // BOM, then @ then ZWNBS (aka BOM), then @
    val cs = CharsetICU.forNameICU("utf-16BE")
    val dn = cs.displayName()
    assertEquals("UTF-16BE", dn)
    val decoder = cs.newDecoder().asInstanceOf[CharsetDecoderICU]
    decoder.setToUCallback(CoderResult.malformedForLength(2), new DecodeMalformedHandler(), this)
    decoder.setToUCallback(CoderResult.unmappableForLength(2), new DecodeUnmappableHandler(), this)
    val inBuf = Array[Int](0xFE, 0xFF, 0x00, 0x40, 0xFE, 0xFF, 0x00, 0x40)
    val input = new ByteArrayInputStream(inBuf);
    val act = Converter.parse(input, decoder)
    assertEquals(exp, act)
  }

  def howManyCallbacks(inBuf: Array[Byte]): Int = {

    val cs = CharsetICU.forNameICU("utf-8")
    val dn = cs.displayName()
    var counter: Int = 0

    assertEquals("UTF-8", dn)

    val decoder = cs.newDecoder().asInstanceOf[CharsetDecoderICU]
    val handler = new CharsetCallback.Decoder {
      def call(decoder: CharsetDecoderICU,
               context: Any,
               source: ByteBuffer,
               target: CharBuffer,
               offsets: IntBuffer,
               buffer: Array[Char],
               length: Int,
               cr: CoderResult): CoderResult = {
        // println("cr = " + cr.toString)

        // 
        // We just want to count how many times this thing
        // gets called in various error situations
        //
        counter += 1
        //
        // We want to return here something that tells ICU to 
        // ignore this error (skip) and just keep going after the malformed sequence.
        //
        CoderResult.UNDERFLOW // Hmmm.. means ok actually. Or so it seems.
        // cr
      }
    }

    // We pass a CoderResult.malformedForLength object to tell 
    // it that's the callback we want to handle. But these malformedForLength objects are also used to 
    // communicate back about exactly how long the malformed sequence of bytes is. But to tell it 
    // that we want to handle malformed it doesn't matter which one we pass. So the 
    // malformedForLength(1) object will do nicely thank you.
    decoder.setToUCallback(CoderResult.malformedForLength(1), handler, this)

    val input = new ByteArrayInputStream(inBuf);
    val act = Converter.parse(input, decoder)
    counter
  }

  @Test def testHowManyCallbacks1() {
    val inBuf = Array[Int](0xFF, 0xFF, 0xFF) // 0xFF is always illegal utf-8
    val count = howManyCallbacks(inBuf)
    assertEquals(3, count)
  }

  @Test def testHowManyCallbacks2() {
    val inBuf = Array[Int](0xC2, 0x00) // a bad 2-byte sequence 2nd byte bad = 1 error
    val count = howManyCallbacks(inBuf)
    assertEquals(1, count)
  }

  // TODO: get these tests working so as to characterize how many callbacks in these particular error
  // situations. 

  // Fails: throws MalformedInputException which is not being caught.
  // I think that's because we are returning from our handler, in a disposition where it 
  // decides based on our return that it should throw rather than continue.
  //  @Test def testHowManyCallbacks3() {
  //    val inBuf = Array[Int](0xE2, 0xA2, 0xCC) // a bad 3-byte sequence 3rd byte bad = 1 error
  //    val count = howManyCallbacks(inBuf)
  //    assertEquals(1, count)
  //  }
  //    
  // Fails: throws MalformedInputException which is not being caught.
  //    @Test def testHowManyCallbacks4() {
  //       val inBuf = Array[Int](0xF0, 0xA4, 0xAD, 0xC2) // a bad 4-byte sequence - 4th byte bad = 1 error
  //       val count = howManyCallbacks(inBuf)
  //       assertEquals(1, count)
  //    }

  /**
   * Here is why the below is 2 error calls.
   * It's a 4-byte sequence. It's broken at byte 2, which isn't a suitable follow on for byte 1.
   * So byte 1 is deemed an error.
   * Then it picks up with byte 2. Turns out CF BF is a valid 2-byte sequence for character U+03FF.
   * Then it should pick up at the last byte. BF by itself is not valid alone or as first byte of
   * a multi-byte character, so that's an error also. Two errors total.
   */
  @Test def testHowManyCallbacks5() { // That's character U+10FFFF, but with an error
    val inBuf = Array[Int](0xF4, 0xCF, 0xBF, 0xBF)
    val count = howManyCallbacks(inBuf)
    assertEquals(2, count)
  }

  def replaceBadCharacters(inBuf: Array[Byte]): String = {
    val cs = CharsetICU.forNameICU("utf-8")
    val dn = cs.displayName()
    var counter: Int = 0
    assertEquals("UTF-8", dn)
    val decoder = cs.newDecoder().asInstanceOf[CharsetDecoderICU]
    decoder.onMalformedInput(CodingErrorAction.REPLACE)
    val input = new ByteArrayInputStream(inBuf);
    val act = Converter.parse(input, decoder)
    act
  }

  def replaceBadCharactersEncoding(s: String): Array[Byte] = {
    val cs = CharsetICU.forNameICU("utf-8")
    val dn = cs.displayName()
    var counter: Int = 0
    assertEquals("UTF-8", dn)
    val encoder = cs.newEncoder().asInstanceOf[CharsetEncoderICU]
    encoder.onMalformedInput(CodingErrorAction.REPLACE)
    val output = new ByteArrayOutputStream();
    Converter.unparse(output, encoder)(s)
    val act = output.toByteArray()
    act
  }

  @Test def testHowManyReplacements1() {
    val inBuf = Array[Int](0xFF, 0xFF, 0xFF) // 0xFF is always illegal utf-8
    val act = replaceBadCharacters(inBuf)
    assertEquals("\uFFFD\uFFFD\uFFFD", act)
  }

  @Test def testHowManyReplacements2() {
    val inBuf = Array[Int](0xC2, 0x40) // a bad 2-byte sequence 2nd byte bad = 1 error
    val act = replaceBadCharacters(inBuf)
    assertEquals("\uFFFD@", act)
  }

  @Test def testHowManyReplacements3() {
    val inBuf = Array[Int](0xE2, 0xA2, 0xCC) // a bad 3-byte sequence 3rd byte bad = 1 error
    val act = replaceBadCharacters(inBuf)
    assertEquals("\uFFFD", act)
  }

  @Test def testHowManyReplacements4() {
    val inBuf = Array[Int](0xF0, 0xA4, 0xAD, 0xC2) // a bad 4-byte sequence - 4th byte bad = 1 error
    val act = replaceBadCharacters(inBuf)
    assertEquals("\uFFFD", act)
  }

  @Test def testHowManyReplacements5() {
    // That's character U+10FFFF, but with an error
    // a bad 4-byte sequence, 2nd byte doesn't go with first.
    // 2nd and 3rd bytes go together, but fourth is illegal on its own. 
    // so 2 errors
    val inBuf = Array[Int](0xF4, 0xCF, 0xBF, 0xBF)
    val act = replaceBadCharacters(inBuf)
    assertEquals("\uFFFD\u03ff\uFFFD", act)
  }

  @Test def testHowManyReplacements6() {
    // Gibberish. 1st byte of a 3-byte sequence, 2nd byte doesn't go with it,
    // but the sequence of 2nd and 3rd byte is valid (U+03FF). 4th byte invalid alone. 
    val inBuf = Array[Int](0xE2, 0xCF, 0xBF, 0xBF) // a bad 4-byte sequence, but bad in 2nd byte. = 3 errors
    val act = replaceBadCharacters(inBuf)
    assertEquals("\uFFFD\u03ff\uFFFD", act)
  }

  /* To Test
   * 
    DONE: utf-8 parsing and 3-byte encoding of a surrogate code-point is found
    DONE: utf-8 unparsing and code-point of an isolated surrogate is to be encoded.
    DONE: utf-8 decoding, and if you assemble the bits the usual way, you get a code point out of range (higher than 0x10FFFF)
    DONE: utf-8 encoding, and code-point to encode is higher than 0x10FFFF.
    DONE: utf-16 and unpaired surrogate code-point
    DONE: utf-16 encoding and a isolated surrogate code point is encountered
    DONE: utf-16 byte-order-marks found not at the beginning of the data
    
    DONE: 'byte' or 'unit' ?? I.e., in UTF-16BE, will ICU error callback occur once for a broken codepoint, or once per byte?
     Will it indicate one unit of error, or two bytes of error?
     (Note: No way for UTF-16 to error here as it tolerates all UTF-16 codepoints and even illegal surrogate combinations.)
    
    In UTF-8 parsing a run of 27 bad bytes causes what failure? 
    Can one substitute one character for these? What about 27 characters?
    UTF-16 variant (26 bad bytes, 13 bad codepoints) Substitute 26 chars? 13 chars, 1 char?
    
   */

}