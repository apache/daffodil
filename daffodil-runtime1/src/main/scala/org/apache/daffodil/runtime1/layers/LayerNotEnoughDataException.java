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

/**
 * LayerNotEnoughDataException is a custom exception class that represents an exception that occurs
 * when there is insufficient data for a layer in a program.
 */
public class LayerNotEnoughDataException extends LayerProcessingException {

    /**
     * Creates a new instance of LayerNotEnoughDataException with the specified
     * number of needed bytes and available bytes.
     *
     * @param numNeededBytes    the number of bytes needed by the layer
     * @param numAvailableBytes the number of bytes available for the layer
     */
    public LayerNotEnoughDataException(int numNeededBytes, int numAvailableBytes) {
        super("Insufficient data. Needed " + numNeededBytes +
                " bytes, but only " + numAvailableBytes + " were available.");
    }
}
