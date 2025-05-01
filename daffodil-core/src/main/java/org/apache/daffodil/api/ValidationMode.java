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

/**
 * Validation modes for validating the resulting infoset against the DFDL schema
 */
public enum ValidationMode {
  /**
   * Turn off all validation against the DFDL schema
   */
  Off(10),

  /**
   * Perform only facet validation
   */
  Limited(20),

  /**
   * Perform full schema validation using Xerces
   */
  Full(30);

  /**
   * Numeric ID of the validation mode
   */
  public final int id;

  private ValidationMode(int id) {
    this.id = id;
  }
}

