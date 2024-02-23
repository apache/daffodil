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

package org.apache.daffodil.layers.runtime1

import java.io.InputStream
import java.io.OutputStream
import java.util.Optional
import scala.collection.JavaConverters._

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.runtime1.layers.api.JLayerLengthKind
import org.apache.daffodil.runtime1.layers.api.Layer
import org.apache.daffodil.runtime1.layers.api.LayerLimiter
import org.apache.daffodil.runtime1.layers.api.LayerPropertyInfo
import org.apache.daffodil.runtime1.layers.api.LayerRuntime
/*
 * This and related classes implement so called "line folding" from
 * IETF RFC 2822 Internet Message Format (IMF), and IETF RFC 5545 iCalendar.
 *
 * There are multiple varieties of line folding, and it is important to
 * be specific about which algorithm.
 *
 * For IMF, unfolding simply removes CRLFs if they are followed by a space or tab.
 * The Folding is more complex however, as CRLFs can only be inserted before
 * a space/tab that appears in the data. If the data has no spaces, then no
 * folding is possible.
 * If there are spaces/tabs, the one closest to position 78 is used unless it is
 * followed by punctuation, in which case a prior space/tab (if it exists) is used.
 * (This preference for spaces not followed by punctuation is optional, it is
 * not required, but is preferred in the IMF RFC.)
 *
 * Note: folding is done by some systems in a manner that does not respect
 * character boundaries - i.e., in utf-8, a multi-byte character sequence may be
 * broken in the middle by insertion of a CRLF. Hence, unfolding initially treats
 * the text as iso-8859-1, i.e., just bytes, and removes CRLFs, then subsequently
 * re-interprets the bytes as the expected charset such as utf-8.
 *
 * IMF is supposed to be US-ASCII, but implementations have gone to 8-bit characters
 * being preserved, so the above problem can occur.
 *
 * IMF has a maximum line length of 998 characters per line excluding the CRLF.
 * The layer will fail (cause a parse error) if a line longer than this is encountered
 * or constructed after unfolding. When unparsing, if a line longer than 998 cannot be
 * folded due to no spaces/tabs being present in it, then it is an unparse error.
 *
 * Note that i/vCalendar, vCard, and MIME payloads held by IMF do not run into
 * the IMF line length issues, in that they have their own line length limits that
 * are smaller than those of IMF, and which do not require accommodation by having
 * pre-existing spaces/tabs in the data. So such data *always* will be short
 * enough lines.
 *
 * For vCard, iCalendar, and vCalendar, the maximum is 75 bytes plus the CRLF, for
 * a total of 77. Folding is inserted by inserting CRLF + a space or tab. The
 * CRLF and the following space or tab are removed to unfold. If data happened to
 * contain a CRLF followed by a space or tab initially, then that will be lost when
 * the data is parsed.
 *
 * For MIME, the maximum line length is 76.
 */

sealed abstract class LineFoldedLayerBase(mode: LineFoldMode)
  extends Layer(
    layerName = mode.dfdlName,
    supportedLayerLengthKinds =
      Seq(JLayerLengthKind.BoundaryMark, JLayerLengthKind.Implicit).asJava,
    supportedLayerLengthUnits = Seq().asJava,
    isRequiredLayerEncoding = false,
    optLayerVariables = Optional.empty(),
  ) {

  /**
   * The layer limiter needs to be different for boundaryMark and implicit cases.
   *  @return a LayerLimiter or Optional.empty to indicate the standard layer limiter implementation should be used.
   */
  override def layerLimiter(layerPropertyInfo: LayerPropertyInfo): Optional[LayerLimiter] =
    layerPropertyInfo.layerLengthKind() match {
      case JLayerLengthKind.Implicit =>
        Optional.empty() // use the default implicit layer length implementation.
      case JLayerLengthKind.BoundaryMark =>
        Optional.of(new LineFoldedLayerBoundaryMarkLimiter(this))
      case JLayerLengthKind.Explicit =>
        Assert.invariantFailed("layerLengthKind 'explicit' not allowed.")
    }

  override def wrapLayerDecoder(jis: InputStream, lr: LayerRuntime): InputStream =
    new LineFoldedInputStream(mode, jis)

  override def wrapLayerEncoder(jos: OutputStream, lr: LayerRuntime): OutputStream =
    new LineFoldedOutputStream(mode, jos)
}

class LineFoldedIMFLayer extends LineFoldedLayerBase(LineFoldMode.IMF)

class LineFoldedICalendarLayer extends LineFoldedLayerBase(LineFoldMode.iCalendar)

class LineFoldedLayerBoundaryMarkLimiter(layer: LineFoldedLayerBase)
  extends BoundaryMarkRegexLimiter {

  /**
   * This is CRLF not followed by space or tab.
   * Note that if the CR is at end-of-data that is NOT a match. (See the $ in the negative lookahead)
   *
   * Note: one must not use capturing groups in these regex.
   */
  override protected def regexForBoundaryMarkMatch: String = "\\r\\n(?!(?:\\t|\\ |$))"

  override protected def maximumLengthBoundaryMark: String = "\r\n"

  //
  // Q: How do we insert a CRLF "not followed by tab or space" when we don't
  // control what follows?
  // A: We don't. This is nature of the format. If what follows could begin
  // with a space or tab, then the format can't use a line-folded layer.
  //

  override protected def boundaryMarkToInsert: String = "\r\n"
}

/**
 * Doesn't enforce 998 max line length limit.
 *
 * This is a state machine, so of course must be used only on a single thread.
 */
class LineFoldedInputStream(mode: LineFoldMode, jis: InputStream) extends InputStream {

  object State extends org.apache.daffodil.lib.util.Enum {
    sealed trait Type extends EnumValueType

    /**
     * No state. Read a character, and if CR, go to state GotCR.
     */
    case object Start extends Type

    /**
     * Read another character and if LF go to state GotCRLF.
     */
    case object GotCR extends Type

    /**
     * Read another character and if SP/TAB then what we do depends on
     * IMF or iCalendar mode.
     *
     * In iCalendar mode we just goto Start, and iterate
     * again. effectively absorbing all the CR, LF, and the sp/tab.
     *
     * In IMF mode we change state to Start, but we return the sp/tab so that
     * we've effectively absorbed the CRLF, but not the space/tab character.
     */
    case object GotCRLF extends Type

    /**
     * We have a single saved character. Return it, go to Start state
     */
    case object Buf1 extends Type

    /**
     * We have 2 saved characters. They must be a LF, then the next character.
     * Return the LF and go to state Buf1.
     */
    case object Buf2 extends Type

    /**
     * Done. Always return -1, stay in state Done
     */
    case object Done extends Type
  }

  private var c: Int = -2
  private var state: State.Type = State.Start

  /**
   * Assumes an ascii-family encoding, but reads it byte at a time regardless
   * of the encoding. This enables it to handle data where a CRLF was inserted
   * to limit line length, and that insertion broke up a multi-byte character.
   *
   * Does not detect errors such as isolated \r or isolated \n. Leaves those
   * alone. Does not care if lines are in fact less than any limit in length.
   *
   * All this does is remove \r\n[\ \t], replacing with just the space or tab.(IMF)
   * or replace with nothing (iCalendar).
   *
   */
  override def read(): Int = {
    import State._
    if (state eq Done) return -1
    while (state != Done) {
      state match {
        case Start => {
          c = jis.read()
          c match {
            case -1 => {
              state = Done
              return -1
            }
            case '\r' => {
              state = GotCR
            }
            case _ => {
              // state stays Start
              return c
            }
          }
        }
        case GotCR => {
          c = jis.read()
          c match {
            case -1 => {
              state = Done
              return '\r'
            }
            case '\n' => {
              state = GotCRLF
            }
            case _ => {
              state = Buf1
              return '\r'
            }
          }
        }
        case GotCRLF => {
          c = jis.read()
          c match {
            case ' ' | '\t' => {
              if (mode == LineFoldMode.IMF) {
                state = Start // absorb the CR, LF, but not the sp/tab
                return c // return the sp/tab
              } else {
                // iCalendar case
                // we don't return a character, we go around again
                // effectively we've absorbed the CR, LF, and the SP/TAB.
                state = Start
              }
            }
            case _ => {
              // CRLF followed by other not sp/tab, or end of data.
              // Note: This can happen when layerLengthKind is 'implicit' because the
              // layer does not end at the first \r\n that is not followed by tab/space.
              // Buffer up to LF.
              state = Buf2
              return '\r' // TODO: Why return \r ?
            }
          }
        }
        case Buf1 => {
          state = Start
          return c
        }
        case Buf2 => {
          state = Buf1
          return '\n'
        }
        case Done =>
          Assert.invariantFailed("Done state not allowed.")
      }
    }
    Assert.invariantFailed("No fall through to here.")
  }
}

class LineFoldedOutputStream(mode: LineFoldMode, jos: OutputStream) extends OutputStream {

  private val (lineLength, breaker) = mode match {
    case LineFoldMode.IMF => (78, "\r\n".getBytes("ascii"))
    case LineFoldMode.iCalendar => (75, "\r\n ".getBytes("ascii")) // CRLF + a space.
  }

  private val line = new StringBuilder(lineLength + breaker.length)

  private def lineBytes: Array[Byte] = line.map { _.toByte }.toArray

  private def lastCharWas(c: Char) = {
    if (line.length == 0) false
    else line.last == c.toByte
  }

  private var closed = false

  override def close(): Unit = {
    if (!closed) {
      if (line.length > 0) jos.write(lineBytes)
      jos.close()
      closed = true
    }
  }

  // TODO: This is broken because an isolated \r or \n is output as \r\n, but without verifying that the next character
  //  is space or tab. If that ends up as a \r\n not followed by a space or tab it has broken the data, as on a reparse
  //  that \r\n will be interpreted as an end-of-layer marker.
  //  So as is, this is only correct if isolated \r or \n are always followed by a space/tab character.
  //  Furthermore, if \r or \n isolated was the last character of the data being unparsed, then that will get converted
  //  to a \r\n at the end of data. (Possibly this is not a big problem, the data was ending anyway?)
  //
  //  TODO: To work properly this state machine needs more than just the line buffer as state. It needs to
  //   suspend the decision about what to do with isolated \r and \n until it sees the next character after (if any)
  //   Note that if write(\n) is the last call, then no subsequent character exists, but we can't know what do do with the
  //   \n until the close() happens. At that point we know there is no following character, and the \n can be
  //   converted to a \r\n. But if the write(\n) call is followed by a write(' ') call, then we output the \r\n and the ' '
  //   character. If the write(\n) call is followed by a write(x) where x is not tab or space, then we must output
  //   \r\n and a ' ' so that this does NOT become an artificial end-of-layer boundary mark.
  override def write(bInt: Int): Unit = {
    Assert.usage(!closed)
    Assert.invariant(line.length <= lineLength)
    Assert.usage(bInt >= 0)
    val b = bInt.toChar
    if (line.length < lineLength) {
      // there's room for more on the line
      b match {
        case '\r' => line += b
        case '\n' if !lastCharWas('\r') => {
          // isolated \n. Output both \r and \n
          // and flush the line
          line += '\r'
          line += '\n'
          jos.write(lineBytes)
          line.clear()
        }
        case '\n' if lastCharWas('\r') => {
          // newline after a CR, regular CRLF case.
          // add and output
          line += '\n'
          jos.write(lineBytes)
          line.clear()
        }
        case c if lastCharWas('\r') => {
          // isolated CR. Output CRLF
          line += '\n'
          jos.write(lineBytes)
          line.clear()
          line += c
        }
        case c => {
          line += c
        }
      }
    } else {
      // buffer is full of non-newline-oriented characters
      // this is a "too-long" line now with the incoming character.
      //
      // Now things get quite different for IMF vs. iCalendar
      //
      mode match {
        case LineFoldMode.iCalendar =>
          tooLongLineICalendar(b)
        case LineFoldMode.IMF =>
          tooLongLineIMF(b)
      }
    }
  }

  /**
   * For iCalendar, we insert a CRLF+space right here, without worry about
   * surrounding context characters.
   */
  private def tooLongLineICalendar(b: Char): Unit = {
    line += '\r'
    line += '\n'
    line += ' '
    jos.write(lineBytes)
    line.clear()
    line += b
  }

  /**
   * For IMF, we can only insert a CRLF if we can find a space or tab to
   * put it before.
   *
   * If there is no space/tab we simply cannot break/wrap the line.
   *
   * This should be the latest sp/tab in the data so the line is wrapped as
   * long as allowed.
   *
   * Ideally it would not be a space immediately before punctuation, as that
   * would affect readability (for people) by starting the next line with a
   * punctuation character. But this is, strictly speaking, unnecessary.
   * (It is recommended by the RFC however, so it is likely expected behavior.)
   */
  private def tooLongLineIMF(b: Char): Unit = {
    var i = line.length - 1
    var done = false
    while (i > 0 && !done) {
      val c = line(i)
      c match {
        case ' ' | '\t' => {
          // TODO: Implement check for punctuation immediately after the space or tab
          //  as that makes this an undesirable place for a line-ending to be inserted.
          done = true
          //
          // i is the index of latest sp/tab
          //
          jos.write(lineBytes, 0, i)
          jos.write(breaker)
          //
          // now the characters from i to the end of the line
          // are part of the next line, including the sp/tab.
          // we have to slide those down to positions 0 to n
          // in the line.
          //
          line.delete(0, i) // slides remaining down.
          line += b
        }
        case _ => // just keep iterating
      }
      i -= 1
    }
    if (!done) {
      // We never found a space/tab. We can't break the line.
      // So we output what we have, and keep going with this
      // new character, and no line break.
      jos.write(lineBytes)
      line.clear()
      line += b
    }
  }
}
