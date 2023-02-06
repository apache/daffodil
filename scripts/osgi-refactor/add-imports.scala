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
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File
import scala.collection.mutable.ListBuffer
import scala.io.Source

object AddImports extends App {
  val imports = Seq(
    ("org.apache.daffodil.lib.api._", "daffodil-runtime1/src/main/scala/org/apache/daffodil/runtime1/api/DFDLParserUnparser.scala"),
    ("org.apache.daffodil.runtime1.dsom.walker._", "daffodil-runtime1/src/main/scala/org/apache/daffodil/runtime1/dpath/NodeInfo.scala"),
    ("org.apache.daffodil.lib.externalvars._", "daffodil-runtime1/src/main/scala/org/apache/daffodil/runtime1/externalvars/ExternalVariablesLoader.scala"),
    ("org.apache.daffodil.udf._", "daffodil-runtime1/src/main/scala/org/apache/daffodil/runtime1/udf/UserDefinedFunctionService.scala"),
    ("org.apache.daffodil.runtime1.layers._", "daffodil-runtime1-layers/src/main/scala/org/apache/daffodil/layers/runtime1/Base64Transformer.scala"),
    ("org.apache.daffodil.runtime1.layers._", "daffodil-runtime1-layers/src/main/scala/org/apache/daffodil/layers/runtime1/ByteSwapTransformer.scala"),
    ("org.apache.daffodil.runtime1.layers._", "daffodil-runtime1-layers/src/main/scala/org/apache/daffodil/layers/runtime1/GZipTransformer.scala"),
    ("org.apache.daffodil.runtime1.layers._", "daffodil-runtime1-layers/src/main/scala/org/apache/daffodil/layers/runtime1/LineFoldedTransformer.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/PackedBinaryUnparserTraits.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/BCDUnparsers.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/SpecifiedLengthUnparsers.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/BinaryBooleanUnparsers.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/BinaryNumberUnparsers.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/BlobLengthUnparser.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/ChoiceAndOtherVariousUnparsers.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/ConvertBinaryCalendarUnparser.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/ConvertNonBaseTenTextNumberUnparser.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/ConvertTextBooleanUnparser.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/ConvertTextCalendarUnparser.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/ConvertTextStandardNumberUnparser.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/ConvertZonedNumberUnparser.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/DelimitedUnparsers.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/DelimiterUnparsers.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/ElementUnparser.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/ExpressionEvaluatingUnparsers.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/FramingUnparsers.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/HexBinaryLengthUnparser.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/HiddenGroupCombinatorUnparser.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/IBM4690PackedDecimalUnparsers.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/LayeredSequenceUnparser.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/NadaUnparser.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/NilEmptyCombinatorUnparsers.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/NilUnparsers.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/PackedDecimalUnparsers.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/SeparatedSequenceUnparsers.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/SequenceUnparserBases.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/SequenceChildUnparsers.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/StreamSplitterMixin.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/StringLengthUnparsers.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/SpecifiedLength2.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/SuppressableSeparatorUnparser.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-runtime1-unparser/src/main/scala/org/apache/daffodil/unparsers/runtime1/UnseparatedSequenceUnparsers.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/dsom/SchemaComponent.scala"),
    ("org.apache.daffodil.runtime1.dpath._", "daffodil-core/src/main/scala/org/apache/daffodil/core/dpath/Conversions.scala"),
    ("org.apache.daffodil.runtime1.dpath._", "daffodil-core/src/main/scala/org/apache/daffodil/core/dpath/DFDLExpressionParser.scala"),
    ("org.apache.daffodil.runtime1.dpath._", "daffodil-core/src/main/scala/org/apache/daffodil/core/dpath/Expression.scala"),
    ("org.apache.daffodil.runtime1.dpath.NodeInfo", "daffodil-core/src/main/scala/org/apache/daffodil/core/dpath/NodeInfoUtils.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/dsom/CompiledExpression.scala"),
    ("org.apache.daffodil.runtime1.dpath.NodeInfo", "daffodil-core/src/main/scala/org/apache/daffodil/core/dsom/CompiledExpression.scala"),
    ("org.apache.daffodil.core.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/runtime1/TermRuntime1Mixin.scala"),
    ("org.apache.daffodil.runtime1.dpath.NodeInfo", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/primitives/PrimitivesExpressions.scala"),
    ("org.apache.daffodil.runtime1.dpath.NodeInfo", "daffodil-core/src/main/scala/org/apache/daffodil/core/dsom/DFDLEscapeScheme.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/dsom/DFDLSchemaFile.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/dsom/DFDLStatementMixin.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/dsom/RealTermMixin.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/dsom/ElementBase.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/dsom/RuntimePropertyMixins.scala"),
    ("org.apache.daffodil.runtime1.dsom.walker._", "daffodil-core/src/main/scala/org/apache/daffodil/core/dsom/walker/AbstractDSOMWalker.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/dsom/Facets.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/dsom/RestrictionUnion.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/dsom/TermEncodingMixin.scala"),
    ("org.apache.daffodil.unparsers.runtime1._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/primitives/SequenceChild.scala"),
    ("org.apache.daffodil.unparsers.runtime1._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/primitives/ChoiceCombinator.scala"),
    ("org.apache.daffodil.unparsers.runtime1._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/primitives/DelimiterAndEscapeRelated.scala"),
    ("org.apache.daffodil.core.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/primitives/PrimitivesExpressions.scala"),
    ("org.apache.daffodil.unparsers.runtime1._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/primitives/SequenceCombinator.scala"),
    ("org.apache.daffodil.unparsers.runtime1._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/primitives/SpecifiedLength.scala"),
    ("org.apache.daffodil.lib.util._", "daffodil-core/src/test/scala/org/apache/daffodil/core/util/TestUtils.scala"),
    ("org.apache.daffodil.core.util._", "daffodil-core/src/test/scala/org/apache/daffodil/core/api/TestAPI1.scala"),
    ("org.apache.daffodil.lib.api._", "daffodil-core/src/test/scala/org/apache/daffodil/core/api/TestForHeapDump.scala"),
    ("org.apache.daffodil.runtime1.api._", "daffodil-core/src/test/scala/org/apache/daffodil/core/api/TestParseIndividualMessages.scala"),
    ("org.apache.daffodil.runtime1.infoset._", "daffodil-core/src/test/scala/org/apache/daffodil/core/infoset/TestInfoset.scala"),
    ("org.apache.daffodil.runtime1.dpath.NodeInfo", "daffodil-core/src/test/scala/org/apache/daffodil/core/dpath/TestDFDLExpressionEvaluation.scala"),
    ("org.apache.daffodil.runtime1.dpath.NodeInfo", "daffodil-core/src/test/scala/org/apache/daffodil/core/dpath/TestDFDLExpressionTree.scala"),
    ("org.apache.daffodil.runtime1.dpath._", "daffodil-core/src/test/scala/org/apache/daffodil/core/dpath/TestDFDLExpressionTree.scala"),
    ("org.apache.daffodil.core.util._", "daffodil-core/src/test/scala/org/apache/daffodil/core/dsom/TestDsomCompiler.scala"),
    ("org.apache.daffodil.core.util._", "daffodil-core/src/test/scala/org/apache/daffodil/core/dsom/TestDsomCompilerUnparse1.scala"),
    ("org.apache.daffodil.core.util._", "daffodil-core/src/test/scala/org/apache/daffodil/core/dsom/TestInputValueCalc.scala"),
    ("org.apache.daffodil.core.util._", "daffodil-core/src/test/scala/org/apache/daffodil/core/dsom/TestInteriorAlignmentElimination.scala"),
    ("org.apache.daffodil.runtime1.debugger._", "daffodil-cli/src/main/scala/org/apache/daffodil/cli/debugger/CLIDebuggerRunner.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "daffodil-core/src/test/scala/org/apache/daffodil/core/dsom/TestIsScannable.scala"),
    ("org.apache.daffodil.core.util._", "daffodil-core/src/test/scala/org/apache/daffodil/core/dsom/TestPolymorphicUpwardRelativeExpressions.scala"),
    ("org.apache.daffodil.core.util._", "daffodil-core/src/test/scala/org/apache/daffodil/core/dsom/TestPropertyScoping.scala"),
    ("org.apache.daffodil.core.util._", "daffodil-core/src/test/scala/org/apache/daffodil/core/dsom/TestSimpleTypeUnions.scala"),
    ("org.apache.daffodil.core.dsom._", "daffodil-core/src/test/scala/org/apache/daffodil/core/util/TestUtils.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "daffodil-core/src/test/scala/org/apache/daffodil/core/dsom/TestSimpleTypeUnions.scala"),
    ("org.apache.daffodil.runtime1.dsom.walker._", "daffodil-core/src/test/scala/org/apache/daffodil/core/dsom/walker/TestDSOMWalker.scala"),
    ("org.apache.daffodil.core.util._", "daffodil-core/src/test/scala/org/apache/daffodil/core/infoset/TestInfoset2.scala"),
    ("org.apache.daffodil.runtime1.infoset._", "daffodil-core/src/test/scala/org/apache/daffodil/core/infoset/TestInfosetCursor.scala"),
    ("org.apache.daffodil.runtime1.infoset._", "daffodil-core/src/test/scala/org/apache/daffodil/core/infoset/TestInfosetCursor1.scala"),
    ("org.apache.daffodil.runtime1.infoset._", "daffodil-core/src/test/scala/org/apache/daffodil/core/infoset/TestInfosetCursorFromReader.scala"),
    ("org.apache.daffodil.runtime1.infoset._", "daffodil-core/src/test/scala/org/apache/daffodil/core/infoset/TestInfosetCursorFromReader2.scala"),
    ("org.apache.daffodil.runtime1.infoset._", "daffodil-core/src/test/scala/org/apache/daffodil/core/infoset/TestInfosetFree.scala"),
    ("org.apache.daffodil.lib.schema.annotation.props._", "daffodil-core/src/test/scala/org/apache/daffodil/core/schema/annotation/props/TestPropertyRuntime.scala"),
    ("org.apache.daffodil.lib.xml._", "daffodil-core/src/test/scala/org/apache/daffodil/core/xml/TestXMLLoaderWithLocation.scala"),
    ("org.apache.daffodil.io.processors.charset._", "daffodil-tdml-lib/src/test/scala/org/apache/daffodil/processors/charset/TestLSBFirstAndUSASCII7BitPacked.scala"),
    ("org.apache.daffodil.tdml.TDMLException", "daffodil-tdml-processor/src/main/scala/org/apache/daffodil/processor/tdml/TDMLInfosetInputter.scala"),
    ("org.apache.daffodil.tdml.processor._", "daffodil-tdml-processor/src/main/scala/org/apache/daffodil/processor/tdml/DaffodilTDMLDFDLProcessor.scala"),
    ("org.apache.daffodil.tdml.processor._", "daffodil-tdml-processor/src/main/scala/org/apache/daffodil/processor/tdml/Runtime2TDMLDFDLProcessor.scala"),
    ("org.apache.daffodil.tdml.Runner", "daffodil-tdml-processor/src/test/scala/org/apache/daffodil/processor/tdml/TestExtVars1.scala"),
    ("org.apache.daffodil.tdml.Runner", "daffodil-tdml-processor/src/test/scala/org/apache/daffodil/processor/tdml/TestTDMLCrossTest.scala"),
    ("org.apache.daffodil.tdml.Runner", "daffodil-tdml-processor/src/test/scala/org/apache/daffodil/processor/tdml/TestTDMLRoundTrips.scala"),
    ("org.apache.daffodil.tdml.Runner", "daffodil-tdml-processor/src/test/scala/org/apache/daffodil/processor/tdml/TestTDMLRunner2.scala"),
    ("org.apache.daffodil.tdml.Runner", "daffodil-tdml-processor/src/test/scala/org/apache/daffodil/processor/tdml/TestTDMLRunnerCommentSyntax.scala"),
    ("org.apache.daffodil.tdml.Runner", "daffodil-tdml-processor/src/test/scala/org/apache/daffodil/processor/tdml/TestTDMLRunnerConfig.scala"),
    ("org.apache.daffodil.tdml.Runner", "daffodil-tdml-processor/src/test/scala/org/apache/daffodil/processor/tdml/TestTDMLRunner.scala"),
    ("org.apache.daffodil.tdml.Runner", "daffodil-tdml-processor/src/test/scala/org/apache/daffodil/processor/tdml/TestTDMLRunner.scala"),
    ("org.apache.daffodil.tdml.Runner", "daffodil-tdml-processor/src/test/scala/org/apache/daffodil/processor/tdml/TestTDMLRunnerTutorial.scala"),
    ("org.apache.daffodil.tdml.Runner", "daffodil-tdml-processor/src/test/scala/org/apache/daffodil/processor/tdml/TestTDMLRunnerWarnings.scala"),
    ("org.apache.daffodil.tdml.Runner", "daffodil-tdml-processor/src/test/scala/org/apache/daffodil/processor/tdml/TestTDMLUnparseCases.scala"),
    ("org.apache.daffodil.tdml.Runner;", "daffodil-tdml-processor/src/test/java/org/apache/daffodil/processor/tdml/TestRunnerFactory.java"),
    ("org.apache.daffodil.tdml.NoRoundTrip$;", "daffodil-tdml-processor/src/test/java/org/apache/daffodil/processor/tdml/TestRunnerFactory.java"),
    ("org.apache.daffodil.tdml._", "daffodil-tdml-processor/src/test/scala/org/apache/daffodil/processor/tdml/TestTDMLCrossTest.scala"),
    ("org.apache.daffodil.tdml._", "daffodil-tdml-processor/src/test/scala/org/apache/daffodil/processor/tdml/TestTDMLRoundTrips.scala"),
    ("org.apache.daffodil.tdml._", "daffodil-tdml-processor/src/test/scala/org/apache/daffodil/processor/tdml/TestTDMLRunner.scala"),
    ("org.apache.daffodil.tdml._", "daffodil-tdml-processor/src/test/scala/org/apache/daffodil/processor/tdml/TestTDMLUnparseCases.scala"),
    ("org.apache.daffodil.tdml.Document", "daffodil-tdml-processor/src/test/scala/org/apache/daffodil/processor/tdml/TestTDMLRunner2.scala"),
  )

  var importsToRemove = Seq(
    ("org.apache.daffodil.core.dsom.walker._", "daffodil-runtime1/src/main/scala/org/apache/daffodil/runtime1/dpath/NodeInfo.scala"),
    ("org.apache.daffodil.dpath._", "daffodil-core/src/main/scala/org/apache/daffodil/core/dsom/DFDLEscapeScheme.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/primitives/DelimiterAndEscapeRelated.scala"),
    ("org.apache.daffodil.runtime1.processors.unparsers._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/primitives/SequenceChild.scala"),
    ("org.apache.daffodil.dpath._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/primitives/PrimitivesExpressions.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "daffodil-core/src/test/scala/org/apache/daffodil/core/util/TestUtils.scala"),
    ("org.apache.daffodil.processor.tdml.tdml.TDMLInfosetInputter", "daffodil-tdml-processor/src/main/scala/org/apache/daffodil/processor/tdml/DaffodilTDMLDFDLProcessor.scala"),
    ("org.apache.daffodil.processor.tdml.tdml.TDMLInfosetOutputter", "daffodil-tdml-processor/src/main/scala/org/apache/daffodil/processor/tdml/DaffodilTDMLDFDLProcessor.scala"),
  )

  var importsToReplace = Seq(
    ("org.apache.daffodil.dpath._", "org.apache.daffodil.core.dpath._", "daffodil-core/src/main/scala/org/apache/daffodil/core/dsom/CompiledExpression.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "org.apache.daffodil.core.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/Grammar.scala"),
    ("org.apache.daffodil.runtime1.dsom.IIUtils.IIMap", "org.apache.daffodil.core.dsom.IIUtils.IIMap", "daffodil-core/src/main/scala/org/apache/daffodil/core/dsom/SchemaDocument.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "org.apache.daffodil.core.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/GrammarMixin.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "org.apache.daffodil.core.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/primitives/PrimitivesDelimiters.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "org.apache.daffodil.core.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/primitives/PrimitivesNil.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "org.apache.daffodil.core.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/primitives/DelimiterAndEscapeRelated.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "org.apache.daffodil.core.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/SequenceGrammarMixin.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "org.apache.daffodil.core.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/ByteOrderMixin.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "org.apache.daffodil.core.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/primitives/PrimitivesTextNumber.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "org.apache.daffodil.core.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/primitives/PrimitivesZoned.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "org.apache.daffodil.core.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/primitives/LayeredSequence.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "org.apache.daffodil.core.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/primitives/SequenceChild.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "org.apache.daffodil.core.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/primitives/SequenceCombinator.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "org.apache.daffodil.core.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/primitives/ChoiceCombinator.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "org.apache.daffodil.core.dsom._", "daffodil-core/src/main/scala/org/apache/daffodil/core/grammar/primitives/Primitives.scala"),
    ("org.apache.daffodil.lib.api._", "org.apache.daffodil.runtime1.api._", "daffodil-tdml-processor/src/main/scala/org/apache/daffodil/processor/tdml/DaffodilTDMLDFDLProcessor.scala"),
    ("org.apache.daffodil.runtime1.infoset.TestInfoset", "org.apache.daffodil.core.infoset.TestInfoset", "daffodil-core/src/test/scala/org/apache/daffodil/core/dpath/TestDFDLExpressionEvaluation.scala"),
    ("org.apache.daffodil.runtime1.dsom._", "org.apache.daffodil.core.dsom._", "daffodil-core/src/test/scala/org/apache/daffodil/core/grammar/TestGrammar.scala"),
    ("org.apache.daffodil.validation.Validators;", "org.apache.daffodil.lib.validation.Validators;", "daffodil-japi/src/test/java/org/apache/daffodil/example/ValidatorSpiExample.java"),
  )

  var importsToAddLast = Seq(
    ("org.apache.daffodil.lib.api._", "daffodil-tdml-processor/src/main/scala/org/apache/daffodil/processor/tdml/DaffodilTDMLDFDLProcessor.scala"),
  )

  def addImports(name: String, file: File) = {
    val source = Source.fromFile(file)
    val output = ListBuffer.empty[String]
    for (line <- source.getLines()) {
      output += line
      if (line.startsWith("package")) {
        output += s"\nimport ${name}"
      }
    }
    source.close

    val out = new BufferedWriter(new FileWriter(file))
    for (line <- output) out.write(line + "\n")
    out.close
  }

  def removeImports(name: String, file: File) = {
    val source = Source.fromFile(file)
    val output = ListBuffer.empty[String]
    for (line <- source.getLines()) {
      if (!line.startsWith(s"import $name")) {
        output += line
      }
    }
    source.close

    val out = new BufferedWriter(new FileWriter(file))
    for (line <- output) out.write(line + "\n")
    out.close
  }

  def replaceImports(nameFrom: String, nameTo: String, file: File) = {
    val source = Source.fromFile(file)
    val output = ListBuffer.empty[String]
    for (line <- source.getLines()) {
      if (line.startsWith(s"import $nameFrom")) {
        output += s"import $nameTo"
      } else {
        output += line
      }
    }
    source.close

    val out = new BufferedWriter(new FileWriter(file))
    for (line <- output) out.write(line + "\n")
    out.close
  }

  val rootDir = args(0)
  for ((name, fileName) <- imports) {
    val file = new File(rootDir, fileName)
    addImports(name, file)
  }
  for ((name, fileName) <- importsToRemove) {
    val file = new File(rootDir, fileName)
    removeImports(name, file)
  }
  for ((nameFrom, nameTo, fileName) <- importsToReplace) {
    val file = new File(rootDir, fileName)
    replaceImports(nameFrom, nameTo, file)
  }
  for ((name, fileName) <- importsToAddLast) {
    val file = new File(rootDir, fileName)
    addImports(name, file)
  }
}
