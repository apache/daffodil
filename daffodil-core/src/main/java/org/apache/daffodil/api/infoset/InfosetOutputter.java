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
 * Abstract class used to determine how the infoset representation should be
 * output from a call to {@code DataProcessor.parse(input:org\.apache\.daffodil* DataProcessor.parse)}.
 * The Daffodil core will call
 * the various methods of this class in an order appropriate to create an
 * infoset representation.
 * <p>
 * Classes that extend InfosetOutputter are not guaranteed to be thread-safe.
 * <p>
 * Note that these functions all throw the generic java.lang.Exception to
 * indicate error. Part of the reason to do this instead of a custom exception
 * (e.g. InfosetOutputterException) is to simplify implementations. If an
 * implementation already throws an exception when there is an error, there is
 * no need to catch it and wrap it in a Daffodil specific exception. This is
 * especially true considering Daffodil will just unwrap the exception and
 * convert it to a SDE. Additionally, because Scala does not have checked
 * exceptions, it can be difficult to ensure all expected exceptions are caught
 * by implementations. This does mean some exceptions that you might normally
 * expect to bubble up and will not, and will instead be turned into an SDE.
 **/
public abstract class InfosetOutputter {

  /**
   * blob directory path which defaults to java temp dir
   */
  private Path blobDirectory = Paths.get(System.getProperty("java.io.tmpdir"));
  /**
   * blob prefix which defaults to daffodil-
   */
  private String blobPrefix = "daffodil-";
  /**
   * blob suffix which defaults to .blob
   */
  private String blobSuffix = ".blob";
  /**
   * list of blob paths output in the infoset
   */
  private List<Path> blobPaths;

  /**
   * Reset the internal state of this InfosetOutputter. This should be called
   * in between calls to the parse method.
   * <p>
   * call to reuse these. When first constructed, no reset call is necessary.
   */
  public abstract void reset();

  /**
   * Called by Daffodil internals to signify the beginning of the infoset.
   * <p>
   *
   * @throws java.lang.Exception if there was an error and Daffodil should stop parsing
   */
  public abstract void startDocument() throws Exception;

  /**
   * Called by Daffodil internals to signify the end of the infoset.
   * <p>
   *
   * @throws java.lang.Exception if there was an error and Daffodil should stop parsing
   */
  public abstract void endDocument() throws Exception;

  /**
   * Called by Daffodil internals to signify the beginning of a simple element.
   * <p>
   *
   * @param diSimple the simple element that is started. Various fields of
   *                 DISimple can be accessed to determine things like the
   *                 value, nil, name, namespace, etc.
   * @throws java.lang.Exception if there was an error and Daffodil should stop parsing
   */
  public abstract void startSimple(InfosetSimpleElement diSimple) throws Exception;

  /**
   * Called by Daffodil internals to signify the end of a simple element.
   * <p>
   *
   * @param diSimple the simple element that is ended. Various fields of
   *                 DISimple can be accessed to determine things like the
   *                 value, nil, name, namespace, etc.
   * @throws java.lang.Exception if there was an error and Daffodil should stop parsing
   */
  public abstract void endSimple(InfosetSimpleElement diSimple) throws Exception;

  /**
   * Called by Daffodil internals to signify the beginning of a complex element.
   * <p>
   *
   * @param complex the complex element that is started. Various fields of
   *                DIComplex can be accessed to determine things like the
   *                nil, name, namespace, etc.
   * @throws java.lang.Exception if there was an error and Daffodil should stop parsing
   */
  public abstract void startComplex(InfosetComplexElement complex) throws Exception;

  /**
   * Called by Daffodil internals to signify the end of a complex element.
   * <p>
   *
   * @param complex the complex element that is ended. Various fields of
   *                DIComplex can be accessed to determine things like the
   *                nil, name, namespace, etc.
   * @throws java.lang.Exception if there was an error and Daffodil should stop parsing
   */
  public abstract void endComplex(InfosetComplexElement complex) throws Exception;

  /**
   * Called by Daffodil internals to signify the beginning of an array of elements.
   * <p>
   *
   * @param array the array that is started. Various fields of
   *              DIArray can be accessed to determine things like the
   *              name, namespace, etc.
   * @throws java.lang.Exception if there was an error and Daffodil should stop parsing
   */
  public abstract void startArray(InfosetArray array) throws Exception;

  /**
   * Called by Daffodil internals to signify the end of an array of elements.
   * <p>
   *
   * @param array the array that is ended. Various fields of
   *              DIArray can be accessed to determine things like the
   *              name, namespace, etc.
   * @throws java.lang.Exception if there was an error and Daffodil should stop parsing
   */
  public abstract void endArray(InfosetArray array) throws Exception;

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

  /**
   * @return blob prefix string
   */
  final public String getBlobPrefix() {
    return blobPrefix;
  }

  /**
   * @return blob suffix string
   */
  final public String getBlobSuffix() {
    return blobSuffix;
  }

  /**
   * @return blob directory path
   */
  final public Path getBlobDirectory() {
    return blobDirectory;
  }

  /**
   * Get the list of blob paths that were output in the infoset.
   * <p>
   * This is the same as what would be found by iterating over the infoset.
   *
   * @return list of blob paths that were output in the infoset.
   */
  final public List<Path> getBlobPaths() {
    return blobPaths;
  }

  /**
   * Set the list of blob paths
   *
   * @param blobPaths list of blob paths to be set
   */
  final public void setBlobPaths(List<Path> blobPaths) {
    this.blobPaths = blobPaths;
  }
}
