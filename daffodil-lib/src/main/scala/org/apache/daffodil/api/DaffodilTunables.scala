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

package org.apache.daffodil.api

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.schema.annotation.props.gen.ParseUnparsePolicy
import org.apache.daffodil.util.LogLevel
import org.apache.daffodil.util.Logging
import org.apache.daffodil.util.Misc
import org.apache.daffodil.xml.DaffodilXMLLoader

object DaffodilTunables {

  def apply(tunables: Map[String, String]): DaffodilTunables = {
    apply().setTunables(tunables)
  }

  def apply(tunable: String, value: String): DaffodilTunables = {
    apply().setTunable(tunable, value)
  }

  def apply(): DaffodilTunables = {
    // override tunables from the global configuration file on the class path, if it exists
    val (configOpt, _) = Misc.getResourceOption("/daffodil-config.xml")
    val configTunables: Map[String, String] =
      if (configOpt.isDefined) {
        val loader = new DaffodilXMLLoader()
        val node = loader.load(new URISchemaSource(configOpt.get))
        val trimmed = scala.xml.Utility.trim(node)
        val tunablesNode = (trimmed \ "tunables").headOption
        val tunablesMap: Map[String, String] = tunablesNode match {
          case None => Map.empty
          case Some(tunableNode) => {
            tunableNode.child.map { n => (n.label, n.text) }.toMap
          }
        }
        tunablesMap
      } else {
        Map.empty
      }

    new DaffodilTunables().setTunables(configTunables)
  }
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
  val maxOccursBounds: Long = Int.MaxValue, // Can be as large as Int.MaxValue
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
  val suppressSchemaDefinitionWarnings: List[WarnID] = Nil,

  // By default, path expressions in Daffodil will only work correctly if path
  // steps are used in an expression defined in the schema when compiled. To
  // enable the use of other expressions (e.g. during debugging, where not all
  // expressions are known at schema compile time), set this tunable to true.
  // This may cause a degredation of performance in path expression evaluation,
  // so this should be avoided when in production. This flag is automatically
  // enabled when debugging is enabled.
  val allowExternalPathExpressions: Boolean = false,

  // Daffodil supports Java 7+. This is essentially all version of java that
  // are in use, so we no longer have any unsupported java versions. This
  // tunable no longer has any affect and is only kept for backwards
  // compatability.
  val errorOnUnsupportedJavaVersion: Boolean = true,

  val maximumSimpleElementSizeInCharacters: Int = 1024 * 1024,
  val initialRegexMatchLimitInCharacters: Int = 64,
  val maximumRegexMatchLengthInCharacters: Int = 1024 * 1024,

  /* Due to differences in the DFDL spec and ICU4J SimpleDateFormat, we must have SimpleDateFormat
   * parse in lenient mode, which allows the year value to overflow with very large years
   * into possibly negative years. These tunables simply make sure that value we get back
   * from SimpleDateFormat are reasonable
   */
  val minValidYear: Int = 0,
  val maxValidYear: Int = 9999)

  extends Serializable
  with Logging {

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
      case "allowexternalpathexpressions" => this.copy(allowExternalPathExpressions = java.lang.Boolean.valueOf(value))
      case "erroronunsupportedjavaversion" => this.copy(errorOnUnsupportedJavaVersion = java.lang.Boolean.valueOf(value))
      case "maximumsimpleelementsizeincharacters" => this.copy(maximumSimpleElementSizeInCharacters = java.lang.Integer.valueOf(value))
      case "initialregexmatchlimitincharacters" => this.copy(initialRegexMatchLimitInCharacters = java.lang.Integer.valueOf(value))
      case "maximumregexmatchlengthincharacters" => this.copy(maximumRegexMatchLengthInCharacters = java.lang.Integer.valueOf(value))
      case _ => {
        log(LogLevel.Warning, "Ignoring unknown tunable: %s", tunable)
        this
      }
    }
  }

  def notSuppressedWarning(warnID: WarnID) =
    !suppressSchemaDefinitionWarnings.contains(warnID) &&
      !suppressSchemaDefinitionWarnings.contains(WarnID.All)

}
