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
 * A checksum layer computes some sort of integer value from a region of the data stream. The term checksum is
 * used generically here to subsume all sorts of CRCs, check digits, data hash, and digest calculations.
 * <p>
 * This base is suitable only for checksums computed over small sections of data, not large data streams or whole large
 * files. The entire region of data the checksum is being computed over, will be pulled into a byte buffer in memory.
 * <p>
 * The resulting checksum is the return value of the compute method.
 * <p>
 * This is delivered into a DFDL variable for use by the DFDL schema. This variable can have any name
 * such as 'crc', 'digest', or 'dataHash'.
 * <p>
 * The derived implementation class must also define a getter method based on the name of the DFDL variable which
 * will be assigned with the checksum value. For example if the checksum is actually a specific digest/hash calculation
 * and the DFDL variable is named 'digest', then this getter must be defined:
 * <p>
 *     int getLayerVariableResult_digest() { return getChecksum() }
 * <p>
 * This will be called automatically to retrieve the integer value that was returned from the `compute` method, and
 * the DFDL variable 'digest' will be assigned that value.
 * <p>
 * The derived class implementing a checksum layer must call
 * <p>
 *     setLength(len) // sets the length in bytes
 * <p>
 * to specify the length of the data region in bytes. Normally this would be called from the layer's implementation of
 * the
 * <p>
 *     void setLayerVariableParameters(...) { }
 * <p>
 * method, which, if defined, is called with arguments populated from DFDL variables with the same name (and compatible type)
 * defined in a DFDL schema with the Layer's target namespace. So, for example if a checksum layer needs to
 * receive a parameter from a DFDL variable named "layerEncoding", the setter would be:
 * <p>
 *     void setLayerVariableParameters(String layerEncoding) {
 *         this.layerEncoding = layerEncoding;
 *     }
 * <p>
 * Beside initializing local members, this setter is also an initializer for the layer class instance. Any exception
 * thrown becomes a Schema Definition Error. If there are no parameter variables, then this setter, with no arguments,
 * can be used purely for initialization.
 */
public abstract class ChecksumLayer extends ChecksumLayerBase {

  public ChecksumLayer(String layerName, String layerTargetNamespace) {
    super(layerName, layerTargetNamespace);
  }

  /**
   * Override to compute the checksum of a buffer of data.
   *
   * @param isUnparse true if the direction is unparsing. Used because in some cases the computed checksum must
   *                  be written into the byte buffer in a specific location.
   * @param byteBuffer the bytes over which the checksum is to be computed. This can be modified, (for example so as
   *                   to embed the computed checksum in the middle of the data somewhere) and the resulting
   *                   bytes become the data that is written when unparsing.
   *                   If the bytes in this buffer are modified by the compute method, those modified bytes are what
   *                   the parsing will parse from, and the unparsing will output.
   * @return the checksum value as an Int (32-bit signed integer)
   */
  public abstract int compute(
    boolean isUnparse,
    ByteBuffer byteBuffer
  );
}
