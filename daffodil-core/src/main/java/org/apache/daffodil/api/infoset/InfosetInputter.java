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

import org.apache.daffodil.runtime1.dpath.NodeInfo;

import java.util.Map;

/**
 * Abstract class used to determine how the infoset representation should be
 * input from a call to {@code DataProcessor.unparse(input* DataProcessor.unparse)}. This uses a Cursor API, such
 * that each call to advance/inspect must update a cursor value, minimizing
 * allocations. Callers of advance/inspect are expected to copy out any
 * information from advanceAccessor and inspectAccessor if they need to retain
 * the information after a call to advance/inspect.
 **/
public abstract class InfosetInputter {

  /**
   * variable used to store whether namespaces are supported by the infoset inputter
   */
  private boolean supportsNamespaces;

  /**
   * Get if this infoset inputter supports namespaces. The return value of
   * getNamespace will be ignored if this is false. Note that if this is false,
   * some infoset representations may fail to unparse if the schema depends on
   * namespace information to differentiate between elements.
   *
   * @return true if this infoset inputter supports namespaces
   */
  public boolean getSupportsNamespaces() {
    return supportsNamespaces;
  }

  /**
   * Set if this infoset inputter supports namespaces. The return value of
   * getNamespace will be ignored if this is false. Note that if this is false,
   * some infoset representations may fail to unparse if the schema depends on
   * namespace information to differentiate between elements.
   *
   * @param supportsNS value to set supportNamespaces to
   */
  public void setSupportsNamespaces(boolean supportsNS) {
    this.supportsNamespaces = supportsNS;
  }

  /**
   * @return the current infoset inputter event type
   */
  public abstract Infoset.InfosetInputterEventType getEventType();

  /**
   * Get the local name of the current event. This will only be called when the
   * current event type is StartElement.
   *
   * @return local name of the current event
   */
  public abstract String getLocalName();

  /**
   * Get the namespace of the current event. This will only be called when the
   * current event type is StartElement. If the InfosetInputter does not
   * support namespaces, this should return null. This may return null to
   * represent no namespaces.
   *
   * @return namespace of the current event
   */
  public abstract String getNamespaceURI();

  /**
   * Get the content of a simple type. This will only be called when the
   * current event type is StartElement and the element is a simple type. If
   * the event contains complex data, it is an error and should throw
   * NonTextFoundInSimpleContentException. If the element does not have any
   * simple content, this should return either null or the empty string.
   *
   * @param primType          prim type to query
   * @param runtimeProperties runtime properties ex stringAsXML
   * @return content of a simple type
   */
  public abstract String getSimpleText(
      NodeInfo.Kind primType,
      Map<String, String> runtimeProperties
  );

  /**
   * Determine if the current event is nilled. This will only be called when
   * the current event type is StartElement.
   *
   * @return null if no nil property is set, which implies the element is not nilled
   * or false if the nil property is set, but it is set to false or true
   * if the nil property is set to true.
   */
  public abstract Boolean isNilled();

  /**
   * @return true if there are remaining events. False otherwise.
   */
  public abstract boolean hasNext();

  /**
   * Move the internal state to the next event.
   */
  public abstract void next();

  /**
   * clean up
   */
  public abstract void fini();
}
