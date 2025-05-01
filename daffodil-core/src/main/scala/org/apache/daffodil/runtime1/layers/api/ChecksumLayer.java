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

import org.apache.daffodil.runtime1.layers.ChecksumLayerBase;

import java.nio.ByteBuffer;

/**
 * A checksum layer computes a numeric value from a region of the data stream.
 * <p>
 * The term checksum is used generically here to subsume all sorts of CRCs, check digits, data hash, and
 * digest calculations.
 * <p>
 * This abstract base is suitable only for checksums computed over small sections of data.
 * It is not for large data streams or whole large files.
 * The entire region of data the checksum is being computed over will be pulled into a byte buffer in memory.
 * <p>
 * The resulting checksum is the return value of the {@link #compute} method.
 * <p>
 * This result is delivered into a DFDL variable for use by the DFDL schema.
 * This DFDL variable can have any name such as 'crc', 'digest', or 'dataHash'.
 * <p>
 * The derived implementation class must also define a getter method based on the name of the DFDL variable which
 * will be assigned with the checksum value.
 * For example if the checksum is actually a specific digest/hash calculation and the DFDL variable is named
 * {@code digest}, then this getter must be defined:
 * <pre>{@code
 *     int getLayerVariableResult_digest() {
 *       return this.digest; // usually returns a data member
 *     }
 * }
 * </pre>
 * This will be called automatically to retrieve the integer value that was returned from the {@code compute} method,
 * and the DFDL variable named {@code digest} will be assigned that value.
 * <p>
 * The derived class implementing a checksum layer must call
 * <pre>{@code
 *     setLength(len); // sets the length in bytes
 * }
 * </pre>
 * to specify the length of the data region in bytes. Normally this would be called from the layer's implementation of
 * the {@code setLayerVariableParameters} method:
 * <pre>{@code
 *     void setLayerVariableParameters(...) {
 *         ...
 *         setLength(len); // len is a constant,
 *                         // or is computed from a parameter variable
 *         ...
 *     }
 * }</pre>
 * See the documentation of the {@link Layer} class for a description of how DFDL variables are passed to the arguments
 * of the {@code setLayerVariableParameters} method.
 * <p>
 * See {@link Layer} for more details about layers generally as most of its documentation is
 * relevant to this derived abstract base class as well.
 * </p>
 */
public abstract class ChecksumLayer extends ChecksumLayerBase {

  /**
   * Base class constructor
   * @param layerName the name of the layer
   * @param layerTargetNamespace the URI that is the target namespace of the layer
   * @throws IllegalArgumentException if arguments are null or do not obey required syntax.
   */
  public ChecksumLayer(String layerName, String layerTargetNamespace) {
    super(layerName, layerTargetNamespace);
  }

  /**
   * Override to compute the checksum of a buffer of data.
   *
   * @param isUnparse true if the direction is unparsing. Used because in some cases the computed checksum must
   *                  be written into the byte buffer in a specific location.
   * @param byteBuffer the bytes over which the checksum is to be computed.
   *                   This byte buffer can be modified, (for example so as to embed the computed checksum in the
   *                   middle of the data somewhere).
   *                   The resulting modified bytes become the data that is read by the DFDL parsing and written
   *                   when unparsing.
   * @return the checksum value as an Int (32-bit signed integer)
   */
  public abstract int compute(
    boolean isUnparse,
    ByteBuffer byteBuffer
  );
}
