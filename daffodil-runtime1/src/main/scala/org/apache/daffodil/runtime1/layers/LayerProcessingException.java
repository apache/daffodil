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
package org.apache.daffodil.runtime1.layers;

import org.apache.daffodil.lib.util.Misc;

import java.util.Objects;

/**
 * LayerProcessingException represents an exception that can occur during the usage of layers in a program.
 */
public class LayerProcessingException extends Exception {

  public LayerProcessingException(String msg, Throwable cause) {
    super(msg, cause);
    if (Objects.isNull(msg)) Objects.requireNonNull(cause);
    if (Objects.isNull(cause)) Objects.requireNonNull(msg);
  }

  public LayerProcessingException(String msg) { this(msg, null); }

  /**
   * When creating an exception with just a cause, we also synthesize some sort of
   * message, because otherwise SLF4J doesn't do so, and you get out errors like
   * '[error] null' because you didn't supply a message.
   * @param cause the cause of this processing exception
   */
  public LayerProcessingException(Throwable cause) {
    this((cause.getMessage()) == null ? Misc.getNameFromClass(cause) : cause.getMessage(), cause); }
}
