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

package org.apache.daffodil.layers.runtime1;

import org.apache.daffodil.api.layers.Layer;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.zip.GZIPInputStream;
import java.util.zip.Deflater;

public final class GZipLayer extends Layer {
  
  private int compressionLevel = Deflater.DEFAULT_COMPRESSION;
  
  public GZipLayer() {
    super("gzip", "urn:org.apache.daffodil.layers.gzip");
    // convert IOExceptions to Processing Errors
    setProcessingErrorException(IOException.class);
  }

  /**
   * Provides the layer's parameter variables to the layer.
   *
   * <p>The compression level controls the trade-off between encoding speed
   * and output size when gzip-compressing data during unparsing. Higher
   * levels produce smaller output but take longer to encode.
   *
   * @param compressionLevel an integer specifying the gzip compression level
   *                         to use when unparsing. Valid values are 0 through
   *                         9, where 0 means no compression and 9 means maximum
   *                         compression, or {@link Deflater#DEFAULT_COMPRESSION}
   *                         ({@code -1}) to use the underlying zlib library's
   *                         default (level 6).
   */
  public void setLayerVariableParameters(int compressionLevel) {
    boolean isValidRange = (compressionLevel >= 0 && compressionLevel <= 9)
            || compressionLevel == Deflater.DEFAULT_COMPRESSION;
    if (!isValidRange) 
      runtimeSchemaDefinitionError("Invalid compression level: " + compressionLevel);
    
    this.compressionLevel = compressionLevel;
  }
  
  @Override
  public InputStream wrapLayerInput(InputStream jis) throws Exception {
    return new GZIPInputStream(jis);
  }

  @Override
  public OutputStream wrapLayerOutput(OutputStream jos) throws Exception {
    return new ConfigurableGZIPOutputStream(jos, compressionLevel);
  }

}
