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
 * output from a call to {@code DataProcessor.parse(input:org\.apache\.daffodil* DataProcessor.parse)}. The Daffodil core will call
 * the various methods of this class in an order appropriate to create an
 * infoset representation.
 * <p>
 * Classes that extend InfosetOutputter are not guaranteed to be thread-safe.
 **/
public abstract class InfosetOutputter extends BlobMethods {

  /**
   * Reset the internal state of this InfosetOutputter. This should be called
   * inbetween calls to the parse method.
   */
  public abstract void reset(); // call to reuse these. When first constructed no reset call is necessary.

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
