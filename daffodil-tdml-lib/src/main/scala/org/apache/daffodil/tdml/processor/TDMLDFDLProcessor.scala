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

package org.apache.daffodil.tdml.processor

import java.nio.file.Path

import org.apache.daffodil.api.validation.{ Validator => JValidator }
import org.apache.daffodil.api.{ DataLocation => JDataLocation }
import org.apache.daffodil.api.{ Diagnostic => JDiagnostic }
import org.apache.daffodil.lib.externalvars.Binding
import org.apache.daffodil.lib.iapi.DaffodilSchemaSource
import org.apache.daffodil.lib.iapi.Diagnostic

object TDML {
  type CompileResult =
    Either[java.util.List[JDiagnostic], (java.util.List[JDiagnostic], TDMLDFDLProcessor)]
}

/**
 * The abstract view of a DFDL processor that the TDML runner needs in order to test
 * that DFDL processor.
 *
 * This is intended to be implemented by classes that wrap around DFDL implementation classes.
 * It should be usable to interface via JNI to C/C++ linkable DFDL processors as well as Java linkable.
 *
 * This interface/trait can depend on Daffodil utilities and libraries, but *cannot* depend on anything
 * about any particular DFDL implementation.
 *
 * This object is stateful. It cannot be shared across threads.
 */
trait AbstractTDMLDFDLProcessorFactory {

  protected type R <: AbstractTDMLDFDLProcessorFactory

  def implementationName: String

  def validateDFDLSchemas: Boolean

  def withValidateDFDLSchemas(bool: Boolean): R

  def withCheckAllTopLevel(checkAllTopLevel: Boolean): R

  def withTunables(tunables: Map[String, String]): R

  def getProcessor(
    schemaSource: DaffodilSchemaSource,
    useSerializedProcessor: Boolean,
    optRootName: Option[String] = None,
    optRootNamespace: Option[String] = None,
    tunables: Map[String, String]
  ): TDML.CompileResult
}

/**
 *  This object is stateful. It cannot be shared across threads.
 */
trait TDMLDFDLProcessor {

  protected type R <: TDMLDFDLProcessor

  def withDebugging(onOff: Boolean): R

  def withTracing(onOff: Boolean): R

  def withDebugger(db: AnyRef): R

  def withValidator(validator: JValidator): R

  def withExternalDFDLVariables(externalVarBindings: Seq[Binding]): R

  def parse(is: java.io.InputStream, lengthLimitInBits: Long): TDMLParseResult

  def unparse(infosetXML: scala.xml.Node, outStream: java.io.OutputStream): TDMLUnparseResult

  def unparse(parseResult: TDMLParseResult, outStream: java.io.OutputStream): TDMLUnparseResult
}

trait TDMLResult {
  def isValidationError: Boolean
  def isProcessingError: Boolean
  def getDiagnostics: Seq[JDiagnostic]

  /**
   * Called once the TDMLRunner has finished analyzing the parse/unparse
   * result. This allows state such as temporary files to be cleaned up.
   * Actually does nothing unless overridden by an implementation.
   */
  def cleanUp(): Unit = { /* Do nothing */ }
}

trait TDMLParseResult extends TDMLResult {
  def addDiagnostic(failure: Diagnostic): Unit
  def getResult: scala.xml.Node
  def getBlobPaths: Seq[Path] = Seq.empty
  def currentLocation: JDataLocation
}

trait TDMLUnparseResult extends TDMLResult {
  def bitPos0b: Long

  def finalBitPos0b: Long
  def isScannable: Boolean
  def encodingName: String
}
