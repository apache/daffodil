package edu.illinois.ncsa.daffodil.dsom

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.processors.charset.CharsetUtils
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters

/**
 * Split this out of AnnotatedMixin for separation of
 * concerns reasons.
 *
 * TODO: move to GrammarMixins.scala, or another file
 * of these sorts of traits that are mixed onto the
 * schema components.
 */
trait EncodingMixin { self: AnnotatedSchemaComponent =>
  /**
   * Character encoding common attributes
   *
   * Note that since encoding can be computed at runtime, we
   * create values to tell us if the encoding is known or not
   * so that we can decide things at compile time when possible.
   */

  lazy val isKnownEncoding = {
    //
    // Be sure we check this. encodingErrorPolicy='error' is harder
    // to support because you have to get decode errors precisely
    // in that case. Means you can't do things like just use
    // a buffered reader since filling the buffer may encounter
    // the error even though the parser won't actually consume
    // that much of the data. 
    //
    schemaDefinitionUnless(defaultEncodingErrorPolicy == EncodingErrorPolicy.Replace,
      "Property encodingErrorPolicy='error' not supported.")
    val isKnown = encoding.isConstant
    if (isKnown) {
      val encName = encoding.constantAsString.toUpperCase()
      if (encName.startsWith("UTF-16")) {
        schemaDefinitionUnless(utf16Width == UTF16Width.Fixed, "Property utf16Width='variable' not supported.")
        //
        // TODO: when runtime encoding is supported, must also check for utf16Width
        // (and error if unsupported then, or just implement it!)
      }
    }
    isKnown
  }

  /**
   * Note that the canonical form for encoding names is all upper case.
   */
  lazy val knownEncodingName = {
    Assert.invariant(isKnownEncoding)
    val res = encoding.constantAsString.toUpperCase()
    res
  }

  lazy val knownEncodingCharset = {
    CharsetUtils.getCharset(knownEncodingName)
  }

  // Really bad idea. Don't save these. Decoders and Encoders are stateful
  // so they can't be precomputed here and reused without all sorts of 
  // thread issues and reset protocols.
  //  lazy val knownEncodingDecoder = {
  //    val decoder = knownEncodingCharset.newDecoder()
  //    decoder
  //  }
  //
  //  lazy val knownEncodingEncoder = {
  //    val encoder = knownEncodingCharset.newEncoder()
  //    encoder
  //  }

  /**
   * When the encoding is known, this tells us the mandatory
   * alignment required. This is always 1 or 8.
   * <p>
   * We only have one non-8-bit encoding right now, but there
   * are some 5, 6, and 9 bit encodings out there.
   */
  lazy val knownEncodingAlignmentInBits = {
    knownEncodingName match {
      case "US-ASCII-7-BIT-PACKED" => 1 // canonical form of encoding names is all upper case
      case _ => 8
    }
  }

  /**
   * enables optimizations and random-access
   *
   * variable-width character sets require scanning to determine
   * their end.
   */
  lazy val knownEncodingIsFixedWidth = {
    // val res = knownEncodingCharset.isFixedWidth
    val res = knownEncodingName match {
      case "US-ASCII" | "ASCII" => true
      case "US-ASCII-7-BIT-PACKED" => true
      case "UTF-8" => false
      case "UTF-16" | "UTF-16LE" | "UTF-16BE" => {
        if (utf16Width == UTF16Width.Fixed) true
        else false
      }
      case "UTF-32" | "UTF-32BE" | "UTF-32LE" => true
      case "ISO-8859-1" => true
      case _ => schemaDefinitionError("Text encoding '%s' is not supported.", knownEncodingName)
    }
    res
  }

  lazy val mustBeAnEncodingWith8BitAlignment = {
    !isKnownEncoding || knownEncodingAlignmentInBits == 8
  }

  lazy val couldBeVariableWidthEncoding = !knownEncodingIsFixedWidth

  lazy val knownEncodingWidthInBits = {
    // knownEncodingCharset.width()
    val res = knownEncodingName match {
      case "US-ASCII" | "ASCII" => 8
      case "US-ASCII-7-BIT-PACKED" => 7 // NOTE! 7-bit characters dense packed. 8th bit is NOT unused. 
      case "UTF-8" => -1
      case "UTF-16" | "UTF-16LE" | "UTF-16BE" => {
        if (utf16Width == UTF16Width.Fixed) 16
        else -1
      }
      case "UTF-32" | "UTF-32BE" | "UTF-32LE" => 32
      case "ISO-8859-1" => 8
      case _ => schemaDefinitionError("Text encoding '%s' is not supported.", knownEncodingName)
    }
    res
  }

  lazy val knownEncodingStringBitLengthFunction = {
    //
    // This will be called at runtime, so let's decide
    // what we can, and return an optimized function that 
    // has characteristics of the encoding wired down.
    //
    if (knownEncodingIsFixedWidth) {
      def stringBitLength(str: String) = str.length * knownEncodingWidthInBits
      stringBitLength _
    } else {
      def stringBitLength(str: String) = {
        // variable width encoding, so we have to convert each character 
        // We assume here that it will be a multiple of bytes
        // that is, that variable-width encodings are all some number
        // of bytes.
        str.getBytes(knownEncodingName).length * 8
      }
      stringBitLength _
    }
  }

  lazy val defaultEncodingErrorPolicy = {
    if (DaffodilTunableParameters.requireEncodingErrorPolicyProperty) {
      encodingErrorPolicy
    } else {
      optionEncodingErrorPolicy.getOrElse(EncodingErrorPolicy.Replace)
    }
  }
}
