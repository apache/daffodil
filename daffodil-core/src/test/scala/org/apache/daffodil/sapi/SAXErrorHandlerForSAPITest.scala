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

package org.apache.daffodil.sapi

import org.apache.daffodil.api.Diagnostic

import org.xml.sax.ErrorHandler
import org.xml.sax.SAXParseException

class SAXErrorHandlerForSAPITest extends ErrorHandler {
  private var _diagnostics: Seq[Diagnostic] = Nil
  private var _isError: Boolean = false

  override def warning(exception: SAXParseException): Unit = {
    _isError = false
    val embeddedDiagnostic = exception.getCause.asInstanceOf[Diagnostic]
    _diagnostics :+= embeddedDiagnostic
  }

  override def error(exception: SAXParseException): Unit = {
    _isError = true
    val embeddedDiagnostic = exception.getCause.asInstanceOf[Diagnostic]
    _diagnostics :+= embeddedDiagnostic
  }

  override def fatalError(exception: SAXParseException): Unit = {
    error(exception)
  }

  def getDiagnostics: Seq[Diagnostic] = _diagnostics

  def isError: Boolean = _isError
}
