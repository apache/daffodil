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
 * Interface that adds diagnostic information to classes that extend it.
 * <p>
 * When a function returns a class that extend this, one should call
 * {@code WithDiagnostics.isError} on that class before performing any further
 * actions. If an error exists, any use of that class, aside from those
 * functions in {@link WithDiagnostics}, is invalid and will result in an
 * Exception.
 */
public interface WithDiagnostics {
  /**
   * Determine if any errors occurred in the creation of the parent object.
   *
   * @return true if it represents an error, false otherwise
   */
  boolean isError();

  /**
   * Get the list of {@link org.apache.daffodil.api.Diagnostic}'s created during the construction of the parent object
   *
   * @return list of {@link org.apache.daffodil.api.Diagnostic}'s. May contain errors or warnings, and so may be non-empty even if {@code WithDiagnostics.isError} is false.
   */
  List<Diagnostic> getDiagnostics();
}
