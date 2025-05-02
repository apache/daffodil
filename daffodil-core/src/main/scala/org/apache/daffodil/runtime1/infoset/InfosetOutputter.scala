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
package org.apache.daffodil.runtime1.infoset

import java.nio.file.Path

import org.apache.daffodil.api.InfosetArray
import org.apache.daffodil.api.InfosetComplexElement
import org.apache.daffodil.api.InfosetSimpleElement
import org.apache.daffodil.api.infoset.{ BlobMethods => JBlobMethods }
import org.apache.daffodil.api.infoset.{ InfosetOutputter => JInfosetOutputter }

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
final class InfosetOutputter(actualOutputter: JInfosetOutputter) extends BlobMethodsMixin {

  /**
   * Reset the internal state of this InfosetOutputter. This should be called
   * in between calls to the parse method.
   */
  def reset(): Unit = actualOutputter.reset()

  /**
   * Called by Daffodil internals to signify the beginning of the infoset.
   *
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   */
  @throws[Exception]
  def startDocument(): Unit = actualOutputter.startDocument()

  /**
   * Called by Daffodil internals to signify the end of the infoset.
   *
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   */
  @throws[Exception]
  def endDocument(): Unit = actualOutputter.endDocument()

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
  def startSimple(diSimple: InfosetSimpleElement): Unit = actualOutputter.startSimple(diSimple)

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
  def endSimple(diSimple: InfosetSimpleElement): Unit = actualOutputter.endSimple(diSimple)

  /**
   * Called by Daffodil internals to signify the beginning of a complex element.
   *
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param complex the complex element that is started. Various fields of
   *                  DIComplex can be accessed to determine things like the
   *                  nil, name, namespace, etc.
   */
  @throws[Exception]
  def startComplex(complex: InfosetComplexElement): Unit = actualOutputter.startComplex(complex)

  /**
   * Called by Daffodil internals to signify the end of a complex element.
   *
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param complex the complex element that is ended. Various fields of
   *                  DIComplex can be accessed to determine things like the
   *                  nil, name, namespace, etc.
   */
  @throws[Exception]
  def endComplex(complex: InfosetComplexElement): Unit = actualOutputter.endComplex(complex)

  /**
   * Called by Daffodil internals to signify the beginning of an array of elements.
   *
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param array the array that is started. Various fields of
   *                  DIArray can be accessed to determine things like the
   *                  name, namespace, etc.
   */
  @throws[Exception]
  def startArray(array: InfosetArray): Unit = actualOutputter.startArray(array)

  /**
   * Called by Daffodil internals to signify the end of an array of elements.
   *
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param array the array that is ended. Various fields of
   *                  DIArray can be accessed to determine things like the
   *                  name, namespace, etc.
   */
  @throws[Exception]
  def endArray(array: InfosetArray): Unit = actualOutputter.endArray(array)

  override def getBlobPaths: java.util.List[Path] = actualOutputter.getBlobPaths

  override def getBlobPrefix: String = actualOutputter.getBlobPrefix

  override def getBlobSuffix: String = actualOutputter.getBlobSuffix

  override def getBlobDirectory: Path = actualOutputter.getBlobDirectory
}

/**
 * An available basic implementation of the BLOB methods.
 * Stores blobs in files in directory identified by Java system property
 * `java.io.tempdir`.
 *
 * FIXME: Scaladoc
 */
trait BlobMethodsMixin extends JBlobMethods
