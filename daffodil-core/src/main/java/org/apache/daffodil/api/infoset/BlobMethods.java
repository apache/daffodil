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

package org.apache.daffodil.api.infoset;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

/**
 * An available basic implementation of the BLOB methods.
 * Stores blobs in files in directory identified by Java system property
 * `java.io.tempdir`.
 */
public abstract class BlobMethods {

  private Path blobDirectory = Paths.get(System.getProperty("java.io.tmpdir"));
  private String blobPrefix = "daffodil-";
  private String blobSuffix = ".blob";
  private List<Path> blobPaths;

  /**
   * Set the attributes for how to create blob files.
   *
   * @param dir    the Path the the directory to create files. If the directory
   *               does not exist, Daffodil will attempt to create it before
   *               writing a blob.
   * @param prefix the prefix string to be used in generating a blob file name
   * @param suffix the suffix string to be used in generating a blob file name
   */
  final public void setBlobAttributes(Path dir, String prefix, String suffix) {
    blobDirectory = dir;
    blobPrefix = prefix;
    blobSuffix = suffix;
  }

  final public String getBlobPrefix() {
    return blobPrefix;
  }

  final public String getBlobSuffix() {
    return blobSuffix;
  }

  final public Path getBlobDirectory() {
    return blobDirectory;
  }

  /**
   * Get the list of blob paths that were output in the infoset.
   * <p>
   * This is the same as what would be found by iterating over the infoset.
   */
  final public List<Path> getBlobPaths() {
    return blobPaths;
  }

  final public void setBlobPaths(List<Path> blobPaths) {
    this.blobPaths = blobPaths;
  }
}