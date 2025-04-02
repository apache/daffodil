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
 * Class containing diagnostic information. This does not need to
 * be implemented by the user.
 */
public abstract class Diagnostic extends Exception {
  /**
   * Constructor to create Diagnostic
   *
   * @param message            diagnostic message
   * @param cause              diagnostic cause
   * @param enableSuppression  boolean to enable suppression of warnings
   * @param writableStackTrace writable stack trace
   */
  public Diagnostic(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
    super(message, cause, enableSuppression, writableStackTrace);
  }

  /**
   * Get all diagnostic information, including mode name, schema context, and data location
   * <p>
   * Future versions of Daffodil will change getMessage to return only the message portion of a
   * diagnostic, matching the behavior of getMessageOnly--users should call toString() instead.
   *
   * @return diagnostic message in string form
   */
  public abstract String getMessage();

  /**
   * Get only the message part of a diagnostic.
   * <p>
   * This does not include mode name, schema context, or data location information
   *
   * @return diagnostic message in string form
   */
  public abstract String getMessageOnly();

  /**
   * Get the diagnostic mode name
   *
   * @return diagnostic mode name in string form
   */
  public abstract String getModeName();

  /**
   * Get a string containing the mode name, message, schema location, and data location combined
   * into a single string
   *
   * @return all diagnostic information as a string
   */
  @Override
  public abstract String toString();


  /**
   * Get data location information relevant to this diagnostic object.
   * <p>
   * For example, this might be a file name, and position within the file.
   *
   * @return list of {@link DataLocation}'s related to this diagnostic
   */
  public abstract List<DataLocation> getDataLocations();

  /**
   * Get schema location information relevant to this diagnostic object.
   * <p>
   * For example, this might be a file name of a schema, and position within the schema file.
   *
   * @return list of {@link LocationInSchemaFile}'s related to this diagnostic.
   */
  public abstract List<? extends LocationInSchemaFile> getLocationsInSchemaFiles();

  /**
   * Determine if a diagnostic object represents an error or something less serious.
   *
   * @return true if it represents an error, false otherwise
   */
  public abstract boolean isError();

  /**
   * Determine if a diagnostic object represents a validation diagnostic.
   * This could be a validation error or a validation warning depending on isError
   *
   * @return true if it represents a validation diagnostic, false otherwise
   */
  public abstract boolean isValidation();
}
