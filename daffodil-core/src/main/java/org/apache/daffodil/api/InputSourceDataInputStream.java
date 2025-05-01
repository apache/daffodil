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

package org.apache.daffodil.api;

import java.io.Closeable;
import java.io.IOException;


/**
 * Provides Daffodil with byte data from an InputStream, ByteBuffer, or byte
 * Array.
 * <p>
 * Note that the InputStream variant has potential overhead due to streaming capabilities and
 * support for files greater than 2GB. In some cases, better performance might come from using
 * the byte array or ByteBuffer variants instead. For example, if your data is already in a byte
 * array, one should use the Array[Byte] or ByteBuffer variants instead of wrapping it in a
 * ByteArrayInputStream. As another example, instead of using a FileInputStream like this:
 * <p>
 * {@code
 * Path path = Paths.get(file);
 * FileInputStream fis = Files.newInputStream(path);
 * InputSourceDataInputStream input = InputSourceDataInputStream(fis);
 * }
 * <p>
 * You might consider mapping the file to a MappedByteBuffer like below, keeping in mind that
 * MappedByteBuffers have size limitations and potentially different performance characteristics
 * depending on the file size and system--it maybe not always be faster than above.
 * <p>
 * {@code
 * Path path = Paths.get(file);
 * long size = Files.size(path);
 * FileChannel fc = FileChannel.open(path, StandardOpenOption.READ);
 * ByteBuffer bb = fc.map(FileChannel.MapMode.READ_ONLY, 0, size);
 * fc.close();
 * InputSourceDataInputStream input = new InputSourceDataInputStream(bb);
 * }
 */
public abstract class InputSourceDataInputStream implements Closeable {
  /**
   * Returns true if the input stream has at least 1 bit of data.
   * <p>
   * Does not advance the position.
   * <p>
   * Returns true immediately if the input stream has available data that
   * has not yet been consumed.
   * <p>
   * On a network input stream, this may block to determine if the stream
   * contains data or is at end-of-data.
   * <p>
   * This is used when parsing multiple elements from a stream to see if there
   * is data or not before calling parse().
   * <p>
   * It may also be used after a parse() operation that is intended to consume
   * the entire data stream (such as for a file) to determine if all data has
   * been consumed or some data is left-over.
   */
  public abstract boolean hasData();

  /**
   * Closes the underlying resource.
   * <p>
   * This method is used to close the underlying resource associated with the current instance.
   * It is typically used to release any system resources that have been acquired during
   * the lifespan of the instance.
   * <p>
   * throws IOException if an I/O error occurs during the close operation.
   */
  public abstract void close() throws IOException;

}