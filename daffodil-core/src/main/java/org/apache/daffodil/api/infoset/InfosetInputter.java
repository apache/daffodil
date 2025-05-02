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
import java.util.Optional;

/**
 * Abstract class used to determine how the infoset representation should be
 * input from a call to {@code DataProcessor.unparse(input* DataProcessor.unparse)}. This uses a Cursor API, such
 * that each call to advance/inspect must update a cursor value, minimizing
 * allocations. Callers of advance/inspect are expected to copy out any
 * information from advanceAccessor and inspectAccessor if they need to retain
 * the information after a call to advance/inspect.
 **/
public abstract class InfosetInputter {

  private boolean supportsNamespaces;

  /**
   * Get if this infoset inputter supports namespaces. The return value of
   * getNamespace will be ignored if this is false. Note that if this is false,
   * some infoset representations may fail to unparse if the schema depends on
   * namespace information to differentiate between elements.
   */
  public boolean getSupportsNamespaces() {
    return supportsNamespaces;
  }

  /**
   * Set if this infoset inputter supports namespaces. The return value of
   * getNamespace will be ignored if this is false. Note that if this is false,
   * some infoset representations may fail to unparse if the schema depends on
   * namespace information to differentiate between elements.
   */
  public void setSupportsNamespaces(boolean supportsNamespaces) {
    this.supportsNamespaces = supportsNamespaces;
  }

  /**
   * Return the current infoset inputter event type
   */
  public abstract Infoset.InfosetInputterEventType getEventType();

  /**
   * Get the local name of the current event. This will only be called when the
   * current event type is StartElement.
   */
  public abstract String getLocalName();

  /**
   * Get the namespace of the current event. This will only be called when the
   * current event type is StartElement. If the InfosetInputter does not
   * support namespaces, this shoud return null. This may return null to
   * represent no namespaces.
   */
  public abstract String getNamespaceURI();

  /**
   * Get the content of a simple type. This will only be called when the
   * current event type is StartElement and the element is a simple type. If
   * the event contains complex data, it is an error and should throw
   * NonTextFoundInSimpleContentException. If the element does not have any
   * simple content, this should return either null or the empty string.
   */
  public String getSimpleText(
      NodeInfo.Kind primType,
      Map<String, String> runtimeProperties
  ) {
    return getSimpleText(primType);
  }

  /**
   * See getSimpleText(primType, runtimeProperties), which has a default
   * implementation to call this function without the runtimeProperties Map
   */
  public abstract String getSimpleText(NodeInfo.Kind primType);

  /**
   * Determine if the current event is nilled. This will only be called when
   * the current event type is StartElement. Return Optional.empty if no
   * nil property is set, which implies the element is not nilled. Return Optional.of(false)
   * if the nil property is set, but it is set to false. Return Optional.of(true)
   * if the nil property is set to true.
   */
  public abstract Optional<Boolean> isNilled();

  /**
   * Return true if there are remaining events. False otherwise.
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