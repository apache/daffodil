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

import org.apache.daffodil.api.InfosetArray;
import org.apache.daffodil.api.InfosetComplexElement;
import org.apache.daffodil.api.InfosetSimpleElement;

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
public abstract class InfosetOutputter extends BlobMethods {

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
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   */
  public abstract void startDocument() throws Exception;

  /**
   * Called by Daffodil internals to signify the end of the infoset.
   * <p>
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   */
  public abstract void endDocument() throws Exception;

  /**
   * Called by Daffodil internals to signify the beginning of a simple element.
   * <p>
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param diSimple the simple element that is started. Various fields of
   *                 DISimple can be accessed to determine things like the
   *                 value, nil, name, namespace, etc.
   */
  public abstract void startSimple(InfosetSimpleElement diSimple) throws Exception;

  /**
   * Called by Daffodil internals to signify the end of a simple element.
   * <p>
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param diSimple the simple element that is ended. Various fields of
   *                 DISimple can be accessed to determine things like the
   *                 value, nil, name, namespace, etc.
   */
  public abstract void endSimple(InfosetSimpleElement diSimple) throws Exception;

  /**
   * Called by Daffodil internals to signify the beginning of a complex element.
   * <p>
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param complex the complex element that is started. Various fields of
   *                DIComplex can be accessed to determine things like the
   *                nil, name, namespace, etc.
   */
  public abstract void startComplex(InfosetComplexElement complex) throws Exception;

  /**
   * Called by Daffodil internals to signify the end of a complex element.
   * <p>
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param complex the complex element that is ended. Various fields of
   *                DIComplex can be accessed to determine things like the
   *                nil, name, namespace, etc.
   */
  public abstract void endComplex(InfosetComplexElement complex) throws Exception;

  /**
   * Called by Daffodil internals to signify the beginning of an array of elements.
   * <p>
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param array the array that is started. Various fields of
   *              DIArray can be accessed to determine things like the
   *              name, namespace, etc.
   */
  public abstract void startArray(InfosetArray array) throws Exception;

  /**
   * Called by Daffodil internals to signify the end of an array of elements.
   * <p>
   * Throws java.lang.Exception if there was an error and Daffodil should stop parsing
   *
   * @param array the array that is ended. Various fields of
   *              DIArray can be accessed to determine things like the
   *              name, namespace, etc.
   */
  public abstract void endArray(InfosetArray array) throws Exception;
}
