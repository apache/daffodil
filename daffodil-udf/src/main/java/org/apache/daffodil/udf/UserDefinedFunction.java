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

import java.io.Serializable;

/**
 * Interface that all User Defined Functions classes must implement.
 *
 * It implements the java.io.Serializable class and must have the
 * UserDefinedFunctionIdentification annotation applied and filled in with the
 * values to be used in the schema.
 *
 * It must also have an evaluate method that contains the functionality it is
 * offering
 *
 * Any "state" variables should be passed in during overloaded
 * UserDefinedFunctionProvider initialization
 *
 */
@UserDefinedFunctionIdentification(
    name = "replace.me",
    namespaceURI = "replace.me")
public interface UserDefinedFunction extends Serializable {
}
