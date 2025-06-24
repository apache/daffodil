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

import scala.xml.NamespaceBinding;

/**
 * Common metadata access for all elements, of simple or complex type.
 */
public interface ElementMetadata extends TermMetadata {
  /**
   * @return the name of this element. In the case of a global/qualified name, this is only the local
   * part of the QName.
   */
  String name();

  /**
   * @return the namespace URI as a string, or null if no namespace.
   */
  String namespace();

  /**
   * @return the namespace bindings needed to construct an XML element from a Daffodil infoset
   * element of simple or complex type.
   */
  NamespaceBinding minimizedScope();

  /**
   * @return the namespace prefix part of the XML QName of this component, or null if there
   * is no prefix defined or no namespace.
   */
  String prefix();

  /**
   * @return true if two or more occurrences are possible.
   * Note that having only 0 or 1 occurrence is not considered an array,
   * but rather an optional element.
   */
  boolean isArray();

  /**
   * @return true if only 0 or 1 occurrence are possible.
   */
  boolean isOptional();

  /**
   * @return the QName string for this element.
   */
  String toQName();

  /**
   * @return true if the element is declared to be nillable.
   */
  boolean isNillable();

  /**
   * Provides access to the runtime properties. This is an extended collection of
   * name-value pairs which are associated with a schema component.
   * <p>
   * Runtime properties are intended to use for new ad-hoc property extensions to
   * DFDL. These name-value pairs are visible to infoset outputters as well.
   *
   * @return a java-compatible map of name-value pairs.
   */
  java.util.Map<String, String> runtimeProperties();

}
