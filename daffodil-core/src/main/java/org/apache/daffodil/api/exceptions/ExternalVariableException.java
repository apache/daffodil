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

package org.apache.daffodil.api.exceptions;

/**
 * This exception will be thrown if an error occurs when setting an external variable. Example of errors include:
 * - Ambiguity in variable to set
 * - Variable definition not found in a schema
 * - Variable value does not have a valid type with regards to the variable type
 * - Variable cannot be set externally
 */
public class ExternalVariableException extends Exception {
  public ExternalVariableException(String message) {
    super(message);
  }
}
