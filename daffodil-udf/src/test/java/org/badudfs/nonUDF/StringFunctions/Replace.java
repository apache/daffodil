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
package org.badudfs.nonUDF.StringFunctions;

import java.io.Serializable;

import org.apache.daffodil.udf.UserDefinedFunctionIdentification;

/**
 * UDF for implementing UserDefinedFunction Negative Unit test
 *
 * Does not implement UserDefinedFunction interface but implements Serializable
 */
@UserDefinedFunctionIdentification(
    name = "replace",
    namespaceURI = "http://example.com/badudfs/StringFunctions")
public class Replace implements Serializable {
  private static final long serialVersionUID = 2619376314947336164L;

  public String evaluate(String orig, String pre, String post) {
    String ret = "";
    if (orig.length() >= pre.length()) {
      ret = orig.replace(pre, post);
    }
    return ret;
  }
}
