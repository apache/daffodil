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

import org.apache.daffodil.api.{DaffodilSchemaSource, DataLocation, ValidationMode, Diagnostic}
import org.apache.daffodil.externalvars.Binding

object TDML {
  type CompileResult = Either[Seq[Diagnostic], (Seq[Diagnostic], TDMLDFDLProcessor)]
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
  */
trait TDMLDFDLProcessorFactory {

  def validateDFDLSchemas: Boolean

  def setValidateDFDLSchemas(bool:Boolean): Unit

  def setCheckAllTopLevel(checkAllTopLevel: Boolean) : Unit

  def setTunables(tunables: Map[String, String]): Unit

  def setExternalDFDLVariables(externalVarBindings: Seq[Binding]): Unit

  def setDistinguishedRootNode(name: String, namespace: String): Unit

  def getProcessor(schemaSource: DaffodilSchemaSource, useSerializedProcessor: Boolean): TDML.CompileResult
}

trait TDMLDFDLProcessor {

  def setValidationMode(validationMode: ValidationMode.Type) : Unit

  def isError: Boolean

  def getDiagnostics: Seq[Diagnostic]

  def parse(is: java.io.InputStream,  lengthLimitInBits: Long) : TDMLParseResult

  def unparse(infosetXML: scala.xml.Node, outStream: java.io.OutputStream): TDMLUnparseResult

  def unparse(parseResult: TDMLParseResult, outStream: java.io.OutputStream): TDMLUnparseResult

}

trait TDMLResult {
  def isValidationError: Boolean
  def isProcessingError: Boolean
  def getDiagnostics: Seq[Diagnostic]

}

trait TDMLParseResult extends TDMLResult {
  def addDiagnostic(failure: Diagnostic) : Unit
  def getResult : scala.xml.Node
  def currentLocation : DataLocation
}

trait TDMLUnparseResult extends TDMLResult {
  def bitPos0b: Long

  def finalBitPos0b: Long
  def isScannable: Boolean
  def encodingName: String
}