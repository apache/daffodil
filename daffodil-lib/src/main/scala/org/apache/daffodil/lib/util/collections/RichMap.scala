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

package org.apache.daffodil.lib.util.collections

import scala.language.implicitConversions

// TODO scala 2.12 phase out
// Define RichMap to add mapValues and filterKeys method
class RichMap[K, V](val underlying: Map[K, V]) extends AnyVal {
  implicit def mapValues[V2](f: V => V2): Map[K, V2] = underlying.map { case (k, v) =>
    k -> f(v)
  }

  implicit def filterKeys(f: K => Boolean): Map[K, V] = underlying.filter { case (k, _) =>
    f(k)
  }
}
