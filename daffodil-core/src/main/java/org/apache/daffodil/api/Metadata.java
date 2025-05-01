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


/**
 * This is the supportable API for access to the RuntimeData structures
 * which provide access to static information about a given compiled schema
 * metadata object.
 * <p>
 * This is used to interface other data processing fabrics to Daffodil
 * data and metadata, by mapping to/from these metadata objects.
 */
public interface Metadata {

  /**
   * Provides the file context of a metadata component. This refers to the specific
   * DFDL schema file where the corresponding DFDL schema text resides corresponding
   * to this metadata object.
   * <p>
   * This is for use in diagnostic messaging. It is not the actual file URI, because
   * those may contain personal-identifying information about the person/acccount and
   * system that compiled the schema. It will provide enough content about the file URI that
   * a user will be able to identify which file, but some prefix of the path
   * components trimmed to make it of a manageable length.
   * <p>
   * Used along with `schemaFileLineNumber`
   * and `schemaFileLineColumnNumber`
   * this can give a precise location in the DFDL schema file.
   *
   * @return a string containing the file information, or null if unknown.
   */
  String schemaFileInfo();

  /**
   * Provides the line number to go with `schemaFileInfo`.
   *
   * @return the line number as a string, or null if unknown.
   */
  Long schemaFileLineNumber();

  /**
   * Provides the column number within the text line, to go with `schemaFileLineNumber`.
   *
   * @return the column number within the text line, as a string, or null if unknown.
   */
  Long schemaFileLineColumnNumber();

  /**
   * The name of the schema component, in a form suitable for diagnostic messages.
   * Unnamed components like sequence or choice groups have a diagnosticDebugName, despite not having
   * any actual name.
   *
   * @return the name of the component, suitable for use in diagnostic messages.
   */
  String diagnosticDebugName();
}
