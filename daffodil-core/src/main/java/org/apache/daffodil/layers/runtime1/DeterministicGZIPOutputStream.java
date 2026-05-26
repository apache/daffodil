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
import java.util.zip.Deflater;
import java.util.zip.GZIPOutputStream;

/**
 * A GZIPOutputStream that uses Deflater BEST_COMPRESSION to produce 
 * output that is consistent across JDKs linked against different zlib
 * implementations (e.g. stock zlib vs. zlib-ng).
 *
 * According to the zlib documentation/logic from deflate.c, 
 * compression levels range from 0 to 9:
 *   Level 0: fastest, no compression
 *   Level 1: some compression but faster than all other compression levels
 *   Level 9: best compression but slowest
 * 
 * As the compression level number increases, speed decreases. BEST_COMPRESSION
 * corresponds to level 9, which is at the slowest end of the spectrum.
 *
 * At DEFAULT_COMPRESSION(level 6) compression level, stock zlib and zlib-ng produce byte-different
 * gzip output for the same input, but with BEST_COMPRESSION(level 9) they produce identical output.
 */
public final class DeterministicGZIPOutputStream extends GZIPOutputStream {
    public DeterministicGZIPOutputStream(OutputStream out) throws IOException {
        super(out);
        // Force maximum compression so that stock zlib and zlib-ng converge
        // on the same output bytes.
        this.def.setLevel(Deflater.BEST_COMPRESSION);
    }
}