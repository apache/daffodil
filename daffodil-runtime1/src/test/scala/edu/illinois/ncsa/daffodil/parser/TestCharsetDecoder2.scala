package edu.illinois.ncsa.daffodil.parser

import java.nio.charset.Charset
import java.nio.ByteBuffer
import java.nio.CharBuffer
import edu.illinois.ncsa.daffodil.exceptions.Assert
import org.junit.Test
import org.junit.Assert._
import edu.illinois.ncsa.daffodil.processors.charset.CharsetUtils

class TestCharsetDecoder2 {

  @Test def testIfHasJava7DecoderBug {
    if (CharsetUtils.hasJava7DecoderBug) fail("Java 7 Decoder bug detected. Daffodil requires Java 8 (or higher)")
  }
}