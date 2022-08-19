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

package org.apache.daffodil.infoset

import java.nio.file.Path
import java.nio.file.Paths

/**
 * Defines the interface for InfosetOutputters.
 *
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
 */
trait InfosetOutputter {

  import Status._

  def status: Status = READY

  /**
   * Reset the internal state of this InfosetOutputter. This should be called
   * inbetween calls to the parse method.
   */
  def reset(): Unit

  /**
   * Called by Daffodil internals to signify the beginning of the infoset.
   *
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   */
  @throws[Exception]
  def startDocument(): Unit

  /**
   * Called by Daffodil internals to signify the end of the infoset.
   *
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   */
  @throws[Exception]
  def endDocument(): Unit

  /**
   * Called by Daffodil internals to signify the beginning of a simple element.
   *
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param diSimple the simple element that is started. Various fields of
   *                 DISimple can be accessed to determine things like the
   *                 value, nil, name, namespace, etc.
   */
  @throws[Exception]
  def startSimple(diSimple: DISimple): Unit

  /**
   * Called by Daffodil internals to signify the end of a simple element.
   *
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param diSimple the simple element that is ended. Various fields of
   *                 DISimple can be accessed to determine things like the
   *                 value, nil, name, namespace, etc.
   */
  @throws[Exception]
  def endSimple(diSimple: DISimple): Unit

  /**
   * Called by Daffodil internals to signify the beginning of a complex element.
   *
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param diComplex the complex element that is started. Various fields of
   *                  DIComplex can be accessed to determine things like the
   *                  nil, name, namespace, etc.
   */
  @throws[Exception]
  def startComplex(diComplex: DIComplex): Unit

  /**
   * Called by Daffodil internals to signify the end of a complex element.
   *
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param diComplex the complex element that is ended. Various fields of
   *                  DIComplex can be accessed to determine things like the
   *                  nil, name, namespace, etc.
   */
  @throws[Exception]
  def endComplex(diComplex: DIComplex): Unit

  /**
   * Called by Daffodil internals to signify the beginning of an array of elements.
   *
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param diComplex the array that is started. Various fields of
   *                  DIArray can be accessed to determine things like the
   *                  name, namespace, etc.
   */
  @throws[Exception]
  def startArray(diArray: DIArray): Unit

  /**
   * Called by Daffodil internals to signify the end of an array of elements.
   *
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param diComplex the array that is ended. Various fields of
   *                  DIArray can be accessed to determine things like the
   *                  name, namespace, etc.
   */
  @throws[Exception]
  def endArray(diArray: DIArray): Unit

  def getStatus(): Status = {
    // Done, Ready (Not started), Visiting (part way done - can retry to visit more)...
    status
  }

  /**
   * Helper function to determine if an element is nilled or not, taking into
   * account whether or not the nilled state has been set yet.
   *
   * @param diElement the element to check the nilled state of
   *
   * @return true if the nilled state has been set and is true. false if the
   *         nilled state is false or if the nilled state has not been set yet
   *         (e.g. during debugging)
   */
  final def isNilled(diElement: DIElement): Boolean = {
    val maybeIsNilled = diElement.maybeIsNilled
    maybeIsNilled.isDefined && maybeIsNilled.get == true
  }

  /**
   * Set the attributes for how to create blob files.
   *
   * @param dir the Path the the directory to create files. If the directory
   *            does not exist, Daffodil will attempt to create it before
   *            writing a blob.
   * @param prefix the prefix string to be used in generating a blob file name
   * @param suffix the suffix string to be used in generating a blob file name
   */
  final def setBlobAttributes(dir: Path, prefix: String, suffix: String): Unit = {
    blobDirectory = dir
    blobPrefix = prefix
    blobSuffix = suffix
  }

  /**
   * Get the list of blob paths that were output in the infoset.
   *
   * This is the same as what would be found by iterating over the infoset.
   */
  final def getBlobPaths(): Seq[Path] = blobPaths


  final def getBlobDirectory(): Path = blobDirectory
  final def getBlobPrefix(): String = blobPrefix
  final def getBlobSuffix(): String = blobSuffix
  final def setBlobPaths(paths: Seq[Path]): Unit = blobPaths = paths
  private var blobDirectory: Path = Paths.get(System.getProperty("java.io.tmpdir"))
  private var blobPrefix: String = "daffodil-"
  private var blobSuffix: String = ".blob"
  private var blobPaths: Seq[Path] = Seq.empty
}

object Status extends Enumeration {
  type Status = Value
  val DONE, READY, VISITING = Value
}
