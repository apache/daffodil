/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation
import edu.illinois.ncsa.daffodil.exceptions.NoSchemaFileLocation
import edu.illinois.ncsa.daffodil.util.PreSerialization

/**
 * To eliminate circularities between RuntimeData objects and the
 * encoding compiled expression, all information derived from encodings
 * must come from one of these objects.
 *
 * That way we can construct this separately, the compilation of the
 * compiled expression for encoding can happily insist on a runtimeData object
 * existing to provide the information it generally needs.
 */

/**
 * Definitions that are the same whether we're in the schema compiler
 * or runtime are on this trait.
 */

trait KnownEncodingMixin { self: ThrowsSDE =>

  def isKnownEncoding: Boolean
  def encoding: CompiledExpression
  def utf16Width: UTF16Width
  def knownEncodingAlignmentInBits: Int

  /**
   * Note that the canonical form for encoding names is all upper case.
   */
  final lazy val knownEncodingName = {
    Assert.invariant(isKnownEncoding)
    val res = encoding.constantAsString.toUpperCase()
    res
  }

  final lazy val knownEncodingCharset = {
    schemaDefinitionUnless(isKnownEncoding, "Runtime expressions for encodings are not supported in this version of Daffodil.")
    new DFDLCharset(knownEncodingName)
  }

  /**
   * enables optimizations and random-access
   *
   * variable-width character sets require scanning to determine
   * their end.
   */
  final lazy val knownEncodingIsFixedWidth = {
    if (!isKnownEncoding) false
    else {
      val res = knownEncodingName.toUpperCase match {
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
  }

  final lazy val knownEncodingWidthInBits = {
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

  final lazy val knownEncodingIsUnicode = {
    if (!isKnownEncoding) { false }
    else { knownEncodingName.toUpperCase.startsWith("UTF") }
  }

  final lazy val mustBeAnEncodingWith8BitAlignment = {
    !isKnownEncoding || knownEncodingAlignmentInBits == 8
  }

  final lazy val couldBeVariableWidthEncoding = !knownEncodingIsFixedWidth

  final def knownEncodingStringBitLength(str: String) = {
    //
    // This will be called at runtime, so let's decide
    // what we can, and return an optimized function that 
    // has characteristics of the encoding wired down.
    //
    if (knownEncodingIsFixedWidth) {
      str.length * knownEncodingWidthInBits
    } else {
      // variable width encoding, so we have to convert each character 
      // We assume here that it will be a multiple of bytes
      // that is, that variable-width encodings are all some number
      // of bytes.
      str.getBytes(knownEncodingName).length * 8
    }
  }
}

/**
 * This is the object we serialize.
 *
 * At compile time we will create an encodingInfo
 * for ourselves supplying as context a schema component.
 *
 * At runtime we will create an encodingInfo supplying as context
 * a TermRuntimeData object.
 */

final class EncodingRuntimeData(
  @transient termRuntimeDataArg: => TermRuntimeData,
  override val schemaFileLocation: SchemaFileLocation,
  val encoding: CompiledExpression,
  val optionUTF16Width: Option[UTF16Width],
  val defaultEncodingErrorPolicy: EncodingErrorPolicy,
  val termChildrenEncodingInfo: Seq[EncodingRuntimeData],
  val summaryEncoding: EncodingLattice,
  val isKnownEncoding: Boolean,
  val isScannable: Boolean,
  override val knownEncodingAlignmentInBits: Int)
  extends KnownEncodingMixin with ImplementsThrowsSDE with PreSerialization {

  lazy val termRuntimeData = termRuntimeDataArg

  override def preSerialization: Any = {
    super.preSerialization
    termRuntimeData
  }

  @throws(classOf[java.io.IOException])
  private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

  /**
   * These lazy values would, ideally all be replaced by Option objects.
   * Some of them cannot be evaluated at all if their preconditions aren't met.
   */

  override lazy val utf16Width = {
    schemaDefinitionUnless(optionUTF16Width.isDefined, "Property utf16Width must be provided.")
    val res = optionUTF16Width.get
    schemaDefinitionUnless(res == UTF16Width.Fixed, "Property utf16Width='variable' not supported.")
    res
  }

  /**
   * no alignment properties that would explicitly create
   * a need to align in a way that is not on a suitable boundary
   * for a character.
   */
  lazy val hasTextAlignment = {
    this.knownEncodingAlignmentInBits == termRuntimeData.alignmentValueInBits
  }
}

