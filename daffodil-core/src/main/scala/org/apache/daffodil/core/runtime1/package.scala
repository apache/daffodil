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

package org.apache.daffodil.core

/**
 * Schema Compiler aspects that are specific to Runtime 1.
 *
 * There are some characteristics of Runtime 1 that are unlikely to be shared
 * by other Daffodil runtime backends. This package is for those things.
 *
 * As an example, Runtime 1 has a streaming unparser. This requires that the schema
 * compiler create appropriate runtime data structures to support this behavior.
 */
package object runtime1 {
  // This object exists for scaladoc purposes.
}
