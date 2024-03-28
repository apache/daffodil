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
package org.apache.daffodil.runtime1.layers

import org.apache.daffodil.runtime1.layers.api.Layer

/**
 * The Layer API is defined in Java. This gives the things that throw more
 * scala-flavored signatures (returns Nothing, not Unit)
 */
trait ScalaLayerHelper { self: Layer =>

  def procError(e: Exception): Nothing = processingError(e).asInstanceOf[Nothing]
  def procError(msg: String): Nothing = processingError(msg).asInstanceOf[Nothing]

  def runtimeSDE(e: Exception): Nothing = runtimeSchemaDefinitionError(e).asInstanceOf[Nothing]
  def runtimeSDE(msg: String): Nothing = runtimeSchemaDefinitionError(msg).asInstanceOf[Nothing]

}
