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

package org.apache.daffodil.lib.util

import scala.collection.mutable

/**
 * Compatibility class for 2.12 and 2.13 since MultiMap and inheritance
 * from class mutable.HashMap have been deprecated in 2.13.
 */
class MultiMap[K, V] {
  private val underlying = mutable.Map.empty[K, mutable.Set[V]]

  def addBinding(key: K, value: V): Unit =
    underlying.getOrElseUpdate(key, mutable.Set.empty) += value

  def addBinding(key: K, values: mutable.Set[V]): Unit = {
    values.foreach(addBinding(key, _))
  }

  def removeBinding(key: K, value: V): Unit =
    underlying.get(key).foreach { values =>
      values -= value
      if (values.isEmpty) underlying -= key
    }

  def get(key: K): Option[mutable.Set[V]] = underlying.get(key)

  def keys: Iterable[K] = underlying.keys

  def iterator: Iterator[(K, mutable.Set[V])] = underlying.iterator

  def filter(func: (K, mutable.Set[V]) => Boolean): MultiMap[K, V] = {
    val filtered = new MultiMap[K, V]
    for ((key, values) <- underlying) {
      if (func(key, values)) {
        filtered.addBinding(key, values)
      }
    }
    filtered
  }

  def map[T](func: (K, mutable.Set[V]) => T): collection.Seq[T] = {
    val ret = mutable.ListBuffer.empty[T]
    for ((key, values) <- underlying) {
      ret.append(func(key, values))
    }
    ret
  }
}
