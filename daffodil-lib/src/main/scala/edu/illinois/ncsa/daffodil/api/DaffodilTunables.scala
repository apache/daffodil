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

package edu.illinois.ncsa.daffodil.api

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ParseUnparsePolicy
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Logging

object DaffodilTunables {

  def apply(tunables: Map[String, String]): DaffodilTunables = {
    new DaffodilTunables().setTunables(tunables)
  }

  def apply(tunable: String, value: String): DaffodilTunables = {
    new DaffodilTunables().setTunable(tunable, value)
  }

  def apply(): DaffodilTunables = new DaffodilTunables()
}

case class DaffodilTunables(
  val maxSkipLengthInBytes: Long = 1024, // applicable to leadingSkip and trailingSkip
  val maxBinaryDecimalVirtualPoint: Int = 200, // Can be as large as Int.MaxValue
  val minBinaryDecimalVirtualPoint: Int = -200, // Can be as small as Int.MinValue
  val generatedNamespacePrefixStem: String = "tns",
  val readerByteBufferSize: Long = 8192,
  //
  // If true, require that the bitOrder property is specified. If false, use a
  // default value for bitOrder if not defined in a schema
  //
  // Looks to be compile-time as it gets 'tunable' from Term.
  //
  val requireBitOrderProperty: Boolean = false,
  //
  // If true, require that the encodingErrorPolicy property is specified. If
  // false, use a default value not defined in a schema
  //
  // Looks to be compile-time as it gets 'tunable' from Term.
  //
  val requireEncodingErrorPolicyProperty: Boolean = false,
  //
  // Whether to compile a schema to support parsing, unparsing, both, or to use
  // the daf:parseUnparsePolicy from the root node. None means to use the
  // policy from the schema, otherwise use whatever the value is
  //
  // Looks to be compile-time. Set in Compiler.
  ///
  val parseUnparsePolicy: Option[ParseUnparsePolicy] = None,
  val maxFieldContentLengthInBytes: Long = 1024 * 1024, // Can be as large as Int.MaxValue
  val defaultInitRegexMatchLimitInChars: Long = 32,
  val maxDataDumpSizeInBytes: Long = 256,
  val maxOccursBounds: Long = 1024, // Can be as large as Int.MaxValue
  //
  // When unexpected text is found where a delimiter is expected, this is the maximum
  // number of bytes (characters) to display when the expected delimiter is a variable
  // length delimiter.
  //
  val maxLengthForVariableLengthDelimiterDisplay: Int = 10, // will display this number of bytes
  //
  // In certain I/O optimized situations (text-only, encodingErrorPolicy='replace', fixed-width encoding)
  // input files larger than this will be mmapped. Input files smaller than this
  // will be simply read using ordinary I/O (because for small files that is just faster).
  // This exists because mmap is more expensive than ordinary I/O for small files.
  //
  val inputFileMemoryMapLowThreshold: Long = 32 * 1024 * 1024, // 32Meg
  //
  // TODO: In the future, when we can stream and handle input larger than the JVM single
  // object limits, input files larger than this will be streamed, i.e., using java.io.InputStream.
  // They will not be memory mapped. A CharBuffer 2x larger may be created, and that
  // cannot exceed the JVM maximum size, so this has to be no bigger (and perhaps quite a bit
  // smaller) than 1/2 the maximum JVM object size.
  //
  // val inputFileMemoryMapHighThreshold: Long = 256 * 1024 * 1024, // 256 Meg
  
  //
  // Initial array buffer size allocated for recurring elements (aka arrays)
  //
  // Applies to InfosetImpl, as such is a run-time thing
  //
  val initialElementOccurrencesHint: Long = 10,
  val unqualifiedPathStepPolicy: UnqualifiedPathStepPolicy.Type = UnqualifiedPathStepPolicy.NoNamespace,
  val suppressSchemaDefinitionWarnings: List[WarnID] = Nil)
  extends Serializable
  with Logging
  with DataStreamLimits {

  /* Appear to be Compile-Time as the tunable is obtained from:
   *  Term, SchemaComponent, Element, etc.
   * 
   *  maxSkipLengthInBytes
   *  maxBinaryDecimalVirtualPoint
   *  minBinaryDecimalVirtualPoint
   *  requireEncodingErrorPolicyProperty
   *  requireBitOrderProperty
   *  generatedNamespacePrefixStem
   *  parseUnparsePolicy
   * 
   * Used by StepQNameFactory.  Appears to get tunable from:
   *  Expressions's DPathCompileInfo
   *  CompiledExpression's ElementRuntimeData
   *  
   *  unqualifiedPathStepPolicy
   * 
   * Appear to be Run-Time as the tunable is obtained from:
   *  PState/UState
   *  
   *  initialElementOccurrencesHint
   *  maxOccursBounds
   *  maxDataDumpSizeInBytes
   *  
   * Only used in Main.scala for DataInputStream:
   * 
   *  maxFieldContentLengthInBytes
   *  
   * DataInputStream objects:
   * 
   *  defaultInitRegexMatchLimitInChars
   *  
   * Unused?
   * 
   *  maxLengthForVariableLengthDelimiterDisplay
   * 
   *  */

  def setTunables(tunables: Map[String, String]): DaffodilTunables = {
    var t = this
    tunables.foreach { case (k, v) => t = t.setTunable(k, v) }
    t
  }

  def setTunable(tunable: String, value: String): DaffodilTunables = {
    tunable.toLowerCase match {
      case "maxfieldcontentlengthinbytes" => this.copy(maxFieldContentLengthInBytes = java.lang.Long.valueOf(value))
      case "defaultinitialregexmatchlimitinchars" => this.copy(defaultInitRegexMatchLimitInChars = java.lang.Long.valueOf(value))
      case "maxdatadumpsizeinbytes" => this.copy(maxDataDumpSizeInBytes = java.lang.Long.valueOf(value))
      case "maxoccursbounds" => this.copy(maxOccursBounds = java.lang.Long.valueOf(value))
      case "maxlengthforvariablelengthdelimiterdisplay" => this.copy(maxLengthForVariableLengthDelimiterDisplay = java.lang.Integer.valueOf(value))
      case "inputfilememorymaplowthreshold" => this.copy(inputFileMemoryMapLowThreshold = java.lang.Long.valueOf(value))
      case "initialelementoccurrenceshint" => this.copy(initialElementOccurrencesHint = java.lang.Long.valueOf(value))
      case "unqualifiedpathsteppolicy" => {
        val policy = value.toLowerCase match {
          case "nonamespace" => UnqualifiedPathStepPolicy.NoNamespace
          case "defaultnamespace" => UnqualifiedPathStepPolicy.DefaultNamespace
          case "preferdefaultnamespace" => UnqualifiedPathStepPolicy.PreferDefaultNamespace
          case _ => Assert.usageError("Unknown value for unqualifiedPathStepPolicy tunable. Value must be \"noNamespace\", \"defaultNamespace\", or \"perferDefaultNamespace\".")
        }
        this.copy(unqualifiedPathStepPolicy = policy)
      }
      case "requirebitorderproperty" => this.copy(requireBitOrderProperty = java.lang.Boolean.valueOf(value))
      case "requireencodingerrorpolicyproperty" => this.copy(requireEncodingErrorPolicyProperty = java.lang.Boolean.valueOf(value))
      case "maxskiplengthinbytes" => this.copy(maxSkipLengthInBytes = java.lang.Long.valueOf(value))
      case "maxbinarydecimalvirtualpoint" => this.copy(maxBinaryDecimalVirtualPoint = java.lang.Integer.valueOf(value))
      case "minbinarydecimalvirtualpoint" => this.copy(minBinaryDecimalVirtualPoint = java.lang.Integer.valueOf(value))
      case "generatednamespaceprefixstem" => this.copy(generatedNamespacePrefixStem = value)
      case "readerbytebuffersize" => this.copy(readerByteBufferSize = java.lang.Long.valueOf(value))
      case "parseunparsepolicy" => {
        val policy = value.toLowerCase match {
          case "parseonly" => Some(ParseUnparsePolicy.ParseOnly)
          case "unparseonly" => Some(ParseUnparsePolicy.UnparseOnly)
          case "both" => Some(ParseUnparsePolicy.Both)
          case "schema" => None
          case _ => Assert.usageError("Unknown value for parseUnparsePolicy tunable. Value must be \"parseOnly\", \"unparseOnly\", \"both\", or \"schema\".")
        }
        this.copy(parseUnparsePolicy = policy)
      }
      case "suppressschemadefinitionwarnings" => {
        val ws = """\s+"""
        // value is whitespace separated list of warning identifier strings
        val warnIDs = value.split(ws).toSeq

        var warningsList: List[WarnID] = Nil
        warnIDs.foreach { warnIDString =>
          {
            WarnID.find(warnIDString) match {
              case None => log(LogLevel.Warning, "Ignoring unknown warning identifier: %s", warnIDString)
              case Some(foundID) => warningsList = foundID :: warningsList
            }
          }
        }
        this.copy(suppressSchemaDefinitionWarnings = warningsList)
      }
      case _ => {
        log(LogLevel.Warning, "Ignoring unknown tunable: %s", tunable)
        this
      }
    }
  }

  def notSuppressedWarning(warnID: WarnID) =
    !suppressSchemaDefinitionWarnings.contains(warnID) &&
      !suppressSchemaDefinitionWarnings.contains(WarnID.All)

  def maximumSimpleElementSizeInBytes: Long = this.maxFieldContentLengthInBytes
  def maximumSimpleElementSizeInCharacters: Long = this.maxFieldContentLengthInBytes
  def maximumForwardSpeculationLengthInBytes: Long = this.maxFieldContentLengthInBytes
  def maximumRegexMatchLengthInCharacters: Long = this.maxFieldContentLengthInBytes
  def defaultInitialRegexMatchLimitInChars: Long = this.defaultInitRegexMatchLimitInChars

}
