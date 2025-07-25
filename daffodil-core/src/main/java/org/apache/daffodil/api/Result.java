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

package org.apache.daffodil.api;

import java.util.List;

/**
 * Interface for Parse and Unparse results
 */
public interface Result {
  /**
   * @return list of diagnostics
   */
  List<Diagnostic> getDiagnostics();

  /**
   * @param diagnostic diagnostic to add to list of diagnostics
   */
  void addDiagnostic(Diagnostic diagnostic);

  /**
   * @return true if in error state
   */
  boolean isError();

  /**
   * @return true if cause of error state is processing error
   */
  boolean isProcessingError();

  /**
   * @return true if cause of error state is validation error
   */
  boolean isValidationError();
}
