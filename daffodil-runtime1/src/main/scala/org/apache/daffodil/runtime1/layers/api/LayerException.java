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
package org.apache.daffodil.runtime1.layers.api;

/**
 * LayerException represents an exception that can occur during the usage of layers in a program.
 * It extends the RuntimeException class, making it an unchecked exception.
 */
public class LayerException extends RuntimeException {

  /**
   * Creates a new instance of LayerException with the specified message and cause.
   *
   * @param msg the detail message. It is saved for later retrieval by the getMessage() method.
   * @param cause the cause. It is saved for later retrieval by the getCause() method.
   *              A null value is permitted, and indicates that the cause is nonexistent or unknown.
   */
  public LayerException(String msg, Throwable cause) { super(msg, cause); }

  public LayerException(String msg) { this(msg, null); }

  public LayerException(Throwable cause) {
    this(null, cause);
  }
}

