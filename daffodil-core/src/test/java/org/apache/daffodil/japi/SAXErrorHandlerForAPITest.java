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

package org.apache.daffodil.japi;

import org.apache.daffodil.api.Diagnostic;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import java.util.ArrayList;

public class SAXErrorHandlerForAPITest implements ErrorHandler {
  private ArrayList<org.apache.daffodil.api.Diagnostic> _diagnostics = new ArrayList<>();
  private Boolean _isError = false;

  @Override
  public void warning(SAXParseException exception) throws SAXException {
    _isError = false;
    Diagnostic embeddedDiagnostic = (Diagnostic) exception.getCause();
    _diagnostics.add(embeddedDiagnostic);
  }

  @Override
  public void error(SAXParseException exception) throws SAXException {
    _isError = true;
    Diagnostic embeddedDiagnostic = (Diagnostic) exception.getCause();
    _diagnostics.add(embeddedDiagnostic);
  }

  @Override
  public void fatalError(SAXParseException exception) throws SAXException {
    error(exception);
  }

  public ArrayList<Diagnostic> getDiagnostics() {
    return _diagnostics;
  }

  public boolean isError() {
    return _isError;
  }
}
