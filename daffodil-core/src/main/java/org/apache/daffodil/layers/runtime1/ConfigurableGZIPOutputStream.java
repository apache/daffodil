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

import java.io.IOException;
import java.io.OutputStream;
import java.util.zip.GZIPOutputStream;

/**
 * A {@link GZIPOutputStream} subclass that allows the caller to specify the
 * compression level explicitly. The standard {@code GZIPOutputStream} uses
 * the JDK's default level (which zlib resolves to level 6), with no
 * mechanism to choose a different level. This subclass gives callers control
 * over the speed-vs-compression-ratio trade-off that the gzip format supports.
 *
 * <p>According to the zlib documentation and the logic in {@code deflate.c},
 * compression levels range from 0 to 9:
 * <ul>
 *   <li>Level 0: no compression (fastest)</li>
 *   <li>Level 1: minimal compression, fastest of the compressing levels</li>
 *   <li>Level 9: best compression, slowest</li>
 * </ul>
 *
 * <p>As the compression level increases, encoding speed decreases. Some
 * reports indicate that level 9 is approximately 10x slower than the default
 * for only about 16% additional compression.
 */
public final class ConfigurableGZIPOutputStream extends GZIPOutputStream {

    /**
     * Creates a {@code ConfigurableGZIPOutputStream} that writes compressed
     * data to the given output stream at the specified compression level.
     *
     * @param out   the output stream to write compressed data to
     * @param level the compression level; an integer in the range 0-9,
     *              or {@link java.util.zip.Deflater#DEFAULT_COMPRESSION}
     *              ({@code -1}) for the default
     * @throws IOException              if an I/O error occurs
     * @throws IllegalArgumentException if {@code level} is not a valid compression level
     */
    public ConfigurableGZIPOutputStream(OutputStream out, int level) throws IOException {
        super(out);
        this.def.setLevel(level);
    }
}