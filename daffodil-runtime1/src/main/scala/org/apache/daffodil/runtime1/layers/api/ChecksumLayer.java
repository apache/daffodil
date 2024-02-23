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

import java.nio.ByteBuffer;

import org.apache.daffodil.runtime1.layers.ChecksumLayerBase;

/**
 * Suitable only for checksums computed over small sections of data, not large data streams or whole files.
 *
 * The entire region of data the checksum is being computed over, will be pulled into a byte buffer in memory.
 */
public abstract class ChecksumLayer extends ChecksumLayerBase {

  public ChecksumLayer(String layerName, String layerNamespace) {
    super(layerName, layerNamespace);
  }

  /**
   * Override to compute the checksum of a buffer of data. The value computed is assigned to the first
   * DFDL variable defined by the layer in the LayerInfo object.
   *
   * @param layerRuntime layer context for the computation
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
    LayerRuntime layerRuntime,
    boolean isUnparse,
    ByteBuffer byteBuffer
  );
}