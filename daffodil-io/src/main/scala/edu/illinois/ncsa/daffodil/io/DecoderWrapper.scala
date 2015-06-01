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
//
//object DecoderWrapper {
//
//  lazy val hasJava7DecoderBug = {
//    val decoder = Charset.forName("utf-8").newDecoder()
//    val bb = ByteBuffer.allocate(6)
//    bb.put(-16.toByte) // invalid first utf-8 byte
//    bb.limit(6).position(0)
//    val cb = CharBuffer.allocate(1)
//    val cr = decoder.decode(bb, cb, true)
//    if (cr.isOverflow &&
//      cb.position == 0 &&
//      bb.position == 0) true
//    else if (cr.isError) false
//    else if (cr.isOverflow &&
//      cb.position == 1 &&
//      bb.position == 1 &&
//      cb.get(0) == this.unicodeReplacementChar) false
//    else
//      Assert.invariantFailed("Unexpected decoder behavior. " + cr)
//  }
//
//  val unicodeReplacementChar = '\uFFFD'
//
//}
//
//final case class DecoderWrapper(val decoder: CharsetDecoder) {
//
//  val unicodeReplacementChar = DecoderWrapper.unicodeReplacementChar
//
//  private val initialDecoderOnMalformedInput = decoder.malformedInputAction()
//  private val initialDecoderOnUnmappable = decoder.unmappableCharacterAction()
//
//  private val hasCodingErrorActionReport: Boolean =
//    initialDecoderOnMalformedInput == CodingErrorAction.REPORT &&
//      initialDecoderOnUnmappable == CodingErrorAction.REPORT
//
//  private val cb2 = CharBuffer.allocate(2)
//  /**
//   * This decode method implements workaround for the
//   * problem in Java 7 when the CharBuffer.remaining == 1.
//   */
//  def decode(bb: ByteBuffer, cb: CharBuffer, noMoreData: Boolean): CoderResult = {
//    if (!DecoderWrapper.hasJava7DecoderBug) return decoder.decode(bb, cb, noMoreData)
//    if (cb.remaining > 1) return decoder.decode(bb, cb, noMoreData)
//    if (cb.remaining == 0) return CoderResult.OVERFLOW
//    // 
//    // So the rest of this is for the case where the cb has exactly room for 1 character
//    Assert.invariant(cb.remaining == 1)
//
//    val initialBBPosition = bb.position
//    val initialCBPosition = cb.position
//
//    val decodeCR =
//      if (hasCodingErrorActionReport) decoder.decode(bb, cb, noMoreData) // avoid the try/catch etc.
//      else
//        try {
//          // save and restore the coding error actions
//          decoder.onMalformedInput(CodingErrorAction.REPORT)
//          decoder.onUnmappableCharacter(CodingErrorAction.REPORT)
//          val res1 = decoder.decode(bb, cb, noMoreData)
//
//          if (res1.isError) {
//            // a malformed or unmappable - but this might not be the very first character.
//            if (cb.position() > initialCBPosition) {
//              // We decoded some characters. Let's return those successfully, as
//              // perhaps the caller doesn't need more than this many.
//              return CoderResult.OVERFLOW
//            }
//            // the very first character decode caused an error
//            if (hasCodingErrorActionReport) return res1
//            // fake an overflow that produced a replacement character.
//            val nBytes = res1.length
//            bb.position(initialBBPosition + nBytes)
//            cb.position(initialCBPosition + 1)
//            cb.put(initialCBPosition, unicodeReplacementChar)
//            return CoderResult.OVERFLOW
//          }
//          Assert.invariant(!res1.isError)
//          // must be an overflow or underflow
//          // Did we get a character, ie., decode successfully? 
//          if (cb.position == initialCBPosition + 1)
//            return res1 // no decode error occurred. Normal.
//
//          // Now we have to work-around the Java 7 bug.
//          // if the cb has only 1 location remaining, 
//          // then on decode errors, java 7 doesn't report.
//          // (At least for the utf-8 decoder.)
//          // Instead you get an overflow, but no character
//          // was created and no bytes consumed
//          Assert.invariant(cb.position == initialCBPosition)
//          // got no character even though we overflowed
//          Assert.invariant(bb.position == initialBBPosition) // and consumed no bytes
//          // So we have to have a char buffer with remaining == 2
//          // use our own
//          cb2.clear
//          // scala bug. If instead of this val, you assign
//          // directly to the decodeCR, then the invariant test after fails
//          // as if the assignment never took place.
//          val res2 = decoder.decode(bb, cb2, noMoreData)
//          Assert.invariant(res2.isError)
//          Assert.invariant(cb2.position == 0)
//          Assert.invariant(bb.position == initialBBPosition)
//          //
//          // now we should have a malformed/unmapped exception 
//          // that we can handle normally.
//          //
//          // either malformed or unmappable.
//          res2
//        } finally {
//          decoder.onMalformedInput(initialDecoderOnMalformedInput)
//          decoder.onUnmappableCharacter(initialDecoderOnUnmappable)
//        }
//    def doReplace = {
//      val nMalformedBytes = decodeCR.length
//      bb.position(initialBBPosition + nMalformedBytes)
//      cb.put(unicodeReplacementChar) // into our original cb.
//      CoderResult.OVERFLOW
//    }
//    def doIgnore = {
//      Assert.usageError("unsupported CodingErrorAction.IGNORE")
//    }
//
//    if (decodeCR.isMalformed) initialDecoderOnMalformedInput match {
//      case CodingErrorAction.REPLACE => return doReplace
//      case CodingErrorAction.REPORT => return decodeCR
//      case CodingErrorAction.IGNORE => return doIgnore
//    }
//    //
//    // if we get here then it must be an unmappable character
//    // 
//    Assert.invariant(decodeCR.isUnmappable)
//    initialDecoderOnMalformedInput match {
//      case CodingErrorAction.REPLACE => return doReplace
//      case CodingErrorAction.REPORT => return decodeCR
//      case CodingErrorAction.IGNORE => return doIgnore
//    }
//    Assert.impossible("should be no fall through to here.")
//  }
//
//  def flush(cb: CharBuffer): CoderResult = decoder.flush(cb)
//
//  def charset() = decoder.charset()
//}