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
package org.apache.daffodil.io.processors.charset

import org.apache.daffodil.lib.util.SimpleNamedServiceLoader

/*
 * Finds all pluggable BitCharsets and makes them available to Daffodil after they have been
 * setup as described in BitsCharsetDefinition.scala
 */
object BitsCharsetDefinitionRegistry {

  private lazy val bitsCharsetDefinitionMap: java.util.Map[String, BitsCharsetDefinition] =
    SimpleNamedServiceLoader
      .loadClass[BitsCharsetDefinition](classOf[BitsCharsetDefinition])

  /**
   * Given name, finds the BitsCharsetDefinition or null if not found
   */
  def find(name: String): Option[BitsCharsetDefinition] = Option(
    bitsCharsetDefinitionMap.get(name)
  )

  def supportedEncodingsString = String.join(", ", bitsCharsetDefinitionMap.keySet)
}
