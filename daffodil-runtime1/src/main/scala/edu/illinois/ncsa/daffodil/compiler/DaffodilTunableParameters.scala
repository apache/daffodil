/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.compiler

import edu.illinois.ncsa.daffodil.dsom.DiagnosticImplMixin

/**
 * Size and length limit constants used by the code, some of which will be tunable
 * by the user. Turning them to lower sizes/lengths may improve performance and
 * diagnostic behavior when a format does not need their full range,
 * both by reducing memory footprint, but
 * also by reducing the amount of time taken to scan to the end of what is allowed
 * and fail (and backtrack to try something else) when, for an example, a delimiter
 * is missing from the data.
 */
object DaffodilTunableParameters {

  /**
   * Exceeding these limits is not a back-trackable parse error. It's more severe
   * than that. But it is not a schema-definition error really either.
   */
  class TunableLimitExceededError(limitName: String, cause: Option[Throwable], msg: String, args: Any*)
    extends Exception("Exceeded " + limitName + ": " + msg.format(args: _*), cause.getOrElse(null))
    with DiagnosticImplMixin {

    def this(limitName: String, msg: String, args: Any*) = this(limitName, None, msg, args: _*)

    def this(limitName: String, cause: Throwable) = this(limitName, Some(cause), "")
  }

  //FIXME: These tunables need to be changable per compilation hence
  //stored on the ProcessorFactory, not global like this. 

  //Some - like time limits, are settable on the processor, not the PF or Compiler.
  //TODO: make tunable via setter call on PF.
  //
  // A few of these seem like runtime limits, but they get compiled into regular expressions
  // that we generate.

  var maxFieldContentLengthInBytes: Long = 1024 * 1024 // Can be as large as Int.MaxValue
  var defaultInitialRegexMatchLimitInChars: Long = 32
  var maxDataDumpSizeInBytes: Long = 256

  var maxOccursBounds: Long = 1024 // Can be as large as Int.MaxValue
  var maxSkipLengthInBytes: Long = 1024 // applicable to leadingSkip and trailingSkip
  var maxBinaryDecimalVirtualPoint: Int = 200 // Can be as large as Int.MaxValue
  var minBinaryDecimalVirtualPoint: Int = -200 // Can be as small as Int.MinValue

  // TODO: want to lift limit of Int.MaxValue, since these are supposed to be Long integers.

  var generatedNamespacePrefixStem = "tns"

  var readerByteBufferSize: Long = 8192

  /**
   * When unexpected text is found where a delimiter is expected, this is the maximum
   * number of bytes (characters) to display when the expected delimiter is a variable
   * length delimiter.
   */
  var maxLengthForVariableLengthDelimiterDisplay: Int = 10 // will display this number of bytes

  /**
   * In certain I/O optimized situations (text-only, encodingErrorPolicy='replace', fixed-width encoding)
   * input files larger than this will be mmapped. Input files smaller than this
   * will be simply read using ordinary I/O (because for small files that is just faster).
   * This exists because mmap is more expensive than ordinary I/O for small files.
   */
  var inputFileMemoryMapLowThreshold: Long = 32 * 1024 * 1024 // 32Meg

  /**
   * TODO: In the future, when we can stream and handle input larger than the JVM single
   * object limits, input files larger than this will be streamed, i.e., using java.io.InputStream.
   * They will not be memory mapped. A CharBuffer 2x larger may be created, and that
   * cannot exceed the JVM maximum size, so this has to be no bigger (and perhaps quite a bit
   * smaller) than 1/2 the maximum JVM object size.
   */
  // var inputFileMemoryMapHighThreshold: Long = 256 * 1024 * 1024 // 256 Meg

  /**
   * If true, require that the bitOrder property is specified. If false, use a
   * default value for bitOrder if not defined in a schema
   */
  var requireBitOrderProperty: Boolean = false

  /**
   * If true, require that the encodingErrorPolicy property is specified. If
   * false, use a default value not defined in a schema
   */
  var requireEncodingErrorPolicyProperty: Boolean = false

  /**
   * Initial array buffer size allocated for recurring elements (aka arrays)
   */
  var initialElementOccurrencesHint: Long = 10

  /**
   * Number of elements in infoset allowed before disabling pretty printing
   * output. Set to 0 to disable pretty printing.
   */
  var prettyPrintElementLimit: Int = 200

}
