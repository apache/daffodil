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

import scala.Option;
import scala.collection.immutable.Seq;

public abstract class Diagnostic extends Exception {

  public Diagnostic(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
    super(message, cause, enableSuppression, writableStackTrace);
  }

  /**
   * Get the diagnostic message
   *
   * @return diagnostic message in string form
   */
  public abstract String getMessage();

  /**
   * Get data location information relevant to this diagnostic object.
   * <p>
   * For example, this might be a file name, and position within the file.
   *
   * @return list of [[DataLocation]]'s related to this diagnostic
   */
  public abstract Seq<DataLocation> getDataLocations();

  /**
   * Get schema location information relevant to this diagnostic object.
   * <p>
   * For example, this might be a file name of a schema, and position within the schema file.
   *
   * @return list of [[LocationInSchemaFile]]'s related to this diagnostic.
   */
  public abstract Seq<LocationInSchemaFile> getLocationsInSchemaFiles();

  /**
   * Determine if a diagnostic object represents an error or something less serious.
   *
   * @return true if it represents an error, false otherwise
   */
  public abstract boolean isError();


  public abstract boolean isValidation();

  /**
   * Get the cause of that cause this diagnostic
   *
   * @return the exception that caused the diagnostic
   */
  public abstract Option<Throwable> getSomeCause();

  /**
   * Get the message that caused this diagnostic
   *
   * @return the message that caused the diagnostic
   */
  public abstract Option<String> getSomeMessage();
}
