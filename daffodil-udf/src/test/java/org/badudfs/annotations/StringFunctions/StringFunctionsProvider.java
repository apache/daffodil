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
package org.badudfs.annotations.StringFunctions;

import org.apache.daffodil.udf.UserDefinedFunction;
import org.apache.daffodil.udf.UserDefinedFunctionProvider;

/**
 * UDF Provider for Negative Unit test
 *
 * Contains classes with invalid or missing annotations
 */
public class StringFunctionsProvider extends UserDefinedFunctionProvider {
  @Override
  public UserDefinedFunction createUserDefinedFunction(String namespace, String name) {
    UserDefinedFunction functionClass = null;

    String nn = String.join("_", namespace, name);

    switch (nn) {
    case "http://example.com/ext/badudfs/stringfunctions_":
      functionClass = new Replace();
      break;
    case "":
      functionClass = new Compare();
      break;
    default:
      functionClass = new FuncB();
      break;
    }
    return functionClass;
  }

  @Override
  public Class<?>[] getUserDefinedFunctionClasses() {
    return new Class<?>[] { Compare.class, FuncB.class, Replace.class };
  }
}
