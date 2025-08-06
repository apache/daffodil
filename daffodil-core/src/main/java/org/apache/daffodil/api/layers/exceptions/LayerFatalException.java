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

package org.apache.daffodil.api.layers.exceptions;

import org.apache.daffodil.lib.util.Misc;

/**
 * This is used to encapsulate runtime exceptions that are thrown out of layer code.
 */
public class LayerFatalException extends RuntimeException {
  /**
   * @param message String for exception
   */
  public LayerFatalException(String message) {
    this(message, null);
  }
  /**
   * When constructed with just a cause, we also pass a message if the cause provides one
   * because the SLF4J logging system doesn't synthesize a message from the cause if only
   * a cause is provided.
   * @param cause throwable for exception
   */
  public LayerFatalException(Throwable cause) {
    this(cause.getMessage() == null ? Misc.getNameFromClass(cause) : cause.getMessage(), cause);
  }

  public LayerFatalException(String message, Throwable cause) {
    super(message, cause);
  }
}