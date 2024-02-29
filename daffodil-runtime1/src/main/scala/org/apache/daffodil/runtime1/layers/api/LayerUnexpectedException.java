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
 * LayerUnexpectedException represents an exception that can occur when an unexpected
 * exception is encountered during layer transformation.
 * It extends the LayerException class, which is an unchecked exception.
 */
public class LayerUnexpectedException extends LayerException {

    /**
     * Creates a new instance of LayerUnexpectedException with the specified layer name and cause.
     *
     * @param layerRuntime the layer runtime as passed to the layer code creating this exception.
     * @param cause the cause of the exception. It is saved for later retrieval by the getCause() method.
     */
    public LayerUnexpectedException(LayerRuntime layerRuntime, Throwable cause) {
        super(layerRuntime, "Unexpected exception in layer transformer '" + layerRuntime.layerName() +
                "'.\nCause: " + cause.getClass().getName() + " Message: " +
                cause.getMessage() + ".", cause);
    }
}
