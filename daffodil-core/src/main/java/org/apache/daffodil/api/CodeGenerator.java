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
 * Source code generation and compilation is performed with a language-specific {@link CodeGenerator}},
 * which must be interrogated for diagnostics to see if each call was successful or not.
 */
public interface CodeGenerator extends WithDiagnostics {
  /**
   * Generates language-specific code from a DFDL schema to parse or unparse data
   *
   * @param outputDir output directory in which to create code directory (codeDir)
   * @return path of newly created directory (codeDir) containing generated code
   */
  os.Path generateCode(String outputDir);

  /**
   * Compiles the generated code in order to run it in a TDML test
   *
   * @param codeDir path of newly created directory containing generated code
   * @return path of newly built executable (exe) compiled from generated code
   */
  os.Path compileCode(os.Path codeDir);
}
