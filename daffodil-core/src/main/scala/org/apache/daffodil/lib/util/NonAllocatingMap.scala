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
import scala.jdk.CollectionConverters._

import org.apache.daffodil.lib.exceptions.Assert

import Maybe._

/**
 * Encapsulates Java Maps with get that returns Maybe[V].
 *
 * This helps two ways. First, we're using java maps, and they
 * don't allocate a Some[V] object or None to indicate hit or miss.
 * So we're avoiding allocation every time we hit.
 *
 * Second we're returning a Maybe[V] so that we're still getting
 * type safety that is verified by the scala compiler.
 */
class NonAllocatingMap[K, V <: AnyRef](javaMap: java.util.Map[K, V]) {

  def get(k: K): Maybe[V] = {
    val got = javaMap.get(k)
    if (got eq null) Nope
    else One(got)
  }

  /**
   * We do not allow null to be put into the map as key nor value.
   */
  def put(k: K, v: V): Unit = {
    k match {
      case ar: AnyRef => Assert.usage(ar ne null)
      case _ => // ok
    }
    Assert.usage(v ne null)
    javaMap.put(k, v)
  }

  /*
   * Everything else a java.util.Map supports is also provided.
   */
  def clear(): Unit = javaMap.clear()
  def containsKey(x: Any): Boolean = javaMap.containsKey(x)
  def containsValue(x: Any): Boolean = javaMap.containsValue(x)
  def entrySet(): mutable.Set[java.util.Map.Entry[K, V]] = javaMap.entrySet().asScala
  def isEmpty(): Boolean = javaMap.isEmpty()
  def keySet(): mutable.Set[K] = javaMap.keySet().asScala
  def putAll(x: java.util.Map[K, V]): Unit = {
    // Call our own method so as to insure no nulls find their way in.
    x.asScala.foreach { case (k, v) => put(k, v) }
  }
  def remove(x: Any): Maybe[V] = Maybe(javaMap.remove(x))
  def size(): Int = javaMap.size()
  def values(): Iterable[V] = javaMap.values().asScala

}
