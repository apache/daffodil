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

package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.processors.charset.CharsetUtils
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.EncodingInfo

/**
 * Split this out of AnnotatedMixin for separation of
 * concerns reasons.
 */
trait EncodingMixin {

  /**
   * Delegates to an encodingInfo object.
   */
  def encodingInfo: EncodingInfo

  lazy val isKnownEncoding = encodingInfo.isKnownEncoding
  lazy val knownEncodingName = encodingInfo.knownEncodingName
  lazy val knownEncodingCharset = encodingInfo.knownEncodingCharset
  lazy val knownEncodingAlignmentInBits = encodingInfo.knownEncodingAlignmentInBits
  lazy val knownEncodingIsFixedWidth = encodingInfo.knownEncodingIsFixedWidth

  lazy val mustBeAnEncodingWith8BitAlignment = encodingInfo.mustBeAnEncodingWith8BitAlignment

  lazy val couldBeVariableWidthEncoding = encodingInfo.couldBeVariableWidthEncoding

  lazy val knownEncodingWidthInBits = encodingInfo.knownEncodingWidthInBits

  lazy val dcharset = encodingInfo.knownEncodingCharset

}

trait RuntimeEncodingMixin
  extends EncodingMixin {

  def context: RuntimeData // parsers all have this defined.
  def encodingInfo: EncodingInfo

  def knownEncodingStringBitLength(str: String) = encodingInfo.knownEncodingStringBitLength(str)

}
