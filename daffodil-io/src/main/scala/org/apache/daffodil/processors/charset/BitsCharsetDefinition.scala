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
package org.apache.daffodil.processors.charset

/**
 * These are the classes which must be dynamically loaded in order to add a charset implementation
 * to Daffodil. All charsets must implement this class and be added to the 
 * org.apache.daffodil.processors.charset.BitsCharsetDefinition file in 
 * daffodil-io/src/main/resources/META-INF/services. name() must return a fully capitalized string
 */
abstract class BitsCharsetDefinition(charset: BitsCharset, alias: Option[String] = None) {
  final def name(): String = alias.getOrElse(charset.name).toUpperCase()
  
  final def charset(): BitsCharset = charset
}
