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

package org.apache.daffodil.udf;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation that must be applied to every UDF in order for it to be considered
 * valid.
 *
 * It must have the name and namespaceURI fields initialized with the namespace
 * and name callers would be expected to use in the schema.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface UserDefinedFunctionIdentification {

  /**
   * Get the local name that identifies the user defined function
   *
   * This value must be a valid XML NCName. It should not include a namespace or namespace prefix.
   *
   * @return the value
   */
  String name();

  /**
   * Get the namespace URI that identifies the user defined function
   *
   * This value must be a valid XML anyURI.
   *
   * @return the value
   */
  String namespaceURI();
}
