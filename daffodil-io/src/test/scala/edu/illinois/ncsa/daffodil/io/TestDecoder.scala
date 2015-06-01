//
// KEEP THIS FILE IN CASE WE HAVE TO GO BACK TO SUPPORTING JAVA 7
//
//package edu.illinois.ncsa.daffodil.io
//
//import java.nio.charset.Charset
//import java.nio.charset.CodingErrorAction
//import java.nio.ByteBuffer
//import java.nio.CharBuffer
//import java.nio.charset.CoderResult
//import java.nio.charset.CharsetDecoder
//import edu.illinois.ncsa.daffodil.exceptions.Assert
//import org.junit.Test
//import org.junit.Assert._
//
///**
// * These unit tests characterize the behavior of charset decoders
// * that we depend upon.
// *
// * There appear to be some bugs in Java 7 that are fixed in Java 8.
// * But to work around them we have a DecoderWrapper class.
// *
// * The bug is specifically associated with
// * a use case we need, which is the case where there is room in the
// * output CharBuffer for exactly 1 character.
// *
// */
//class TestDecoder {
//
//  @Test def testDecoder1 {
//    val originalDecoder = Charset.forName("utf-8").newDecoder()
//    originalDecoder.onMalformedInput(CodingErrorAction.REPLACE)
//    originalDecoder.onUnmappableCharacter(CodingErrorAction.REPLACE)
//    val decoder = DecoderWrapper(originalDecoder)
//    val bb = ByteBuffer.wrap("abcdef".getBytes("utf-8"))
//    val cb = CharBuffer.allocate(1)
//    var decodeCR = decoder.decode(bb, cb, true)
//    assertEquals(CoderResult.OVERFLOW, decodeCR)
//    var flushCR = decoder.flush(cb)
//    assertEquals(CoderResult.UNDERFLOW, flushCR)
//    assertEquals(1, cb.position)
//    assertEquals(1, bb.position)
//    cb.flip
//    assertEquals('a', cb.get())
//  }
//
//  @Test def testDecoder2 {
//    val originalDecoder = Charset.forName("utf-8").newDecoder()
//    originalDecoder.onMalformedInput(CodingErrorAction.REPORT)
//    originalDecoder.onUnmappableCharacter(CodingErrorAction.REPORT)
//    val decoder = DecoderWrapper(originalDecoder)
//    val bb = ByteBuffer.allocate(6)
//    bb.put(-16.toByte) // invalid first utf-8 byte
//    bb.put(0.toByte)
//    bb.put(0.toByte)
//    bb.put(0.toByte)
//    bb.put(0.toByte)
//    bb.put(0.toByte)
//    bb.flip
//    val cb = CharBuffer.allocate(1) // allow room for exactly one character.
//    val decodeCR = decoder.decode(bb, cb, true)
//    assertEquals(CoderResult.malformedForLength(1), decodeCR)
//    val flushCR = decoder.flush(cb)
//    assertEquals(CoderResult.UNDERFLOW, flushCR)
//    assertEquals(0, cb.position)
//    assertEquals(0, bb.position)
//  }
//
//  @Test def testDecoder3 {
//    val originalDecoder = Charset.forName("utf-8").newDecoder()
//    originalDecoder.onMalformedInput(CodingErrorAction.REPLACE)
//    originalDecoder.onUnmappableCharacter(CodingErrorAction.REPLACE)
//    val decoder = DecoderWrapper(originalDecoder)
//    val bb = ByteBuffer.allocate(6)
//    bb.put(-16.toByte) // invalid first utf-8 byte
//    bb.put(0.toByte)
//    bb.put(0.toByte)
//    bb.put(0.toByte)
//    bb.put(0.toByte)
//    bb.put(0.toByte)
//    bb.flip
//    val cb = CharBuffer.allocate(1) // allow room for exactly one character.
//    val decodeCR = decoder.decode(bb, cb, true)
//    assertEquals(CoderResult.OVERFLOW, decodeCR)
//    val flushCR = decoder.flush(cb)
//    assertEquals(CoderResult.UNDERFLOW, flushCR)
//    assertEquals(1, cb.position)
//    assertEquals(1, bb.position)
//    cb.flip
//    assertEquals(decoder.unicodeReplacementChar, cb.get())
//  }
//
//  @Test def testDecoder4 {
//    val originalDecoder = Charset.forName("utf-8").newDecoder()
//    originalDecoder.onMalformedInput(CodingErrorAction.REPLACE)
//    originalDecoder.onUnmappableCharacter(CodingErrorAction.REPLACE)
//    val decoder = DecoderWrapper(originalDecoder)
//    val bb = ByteBuffer.wrap("日".getBytes("utf-8"))
//    val cb = CharBuffer.allocate(1) // allow room for exactly one character.
//    val decodeCR = decoder.decode(bb, cb, true)
//    //
//    // If all available bytes are used up, then we get an UNDERFLOW even if 
//    // every location in the output is filled in. Because until we get more bytes,
//    // we can't be overflowing the input buffer. 
//    //
//    assertEquals(CoderResult.UNDERFLOW, decodeCR)
//    val flushCR = decoder.flush(cb)
//    assertEquals(CoderResult.UNDERFLOW, flushCR)
//    assertEquals(1, cb.position)
//    assertEquals(3, bb.position)
//    cb.flip
//    assertEquals('日', cb.get())
//  }
//}