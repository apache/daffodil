/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package org.apache.daffodil.util

import org.apache.daffodil.exceptions.Assert
import Maybe._
import scala.collection.JavaConversions._
import scala.collection.mutable

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
  def put(k: K, v: V) {
    k match {
      case ar: AnyRef => Assert.usage(ar ne null)
      case _ => //ok
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
  def entrySet(): mutable.Set[java.util.Map.Entry[K, V]] = javaMap.entrySet()
  def isEmpty(): Boolean = javaMap.isEmpty()
  def keySet(): mutable.Set[K] = javaMap.keySet()
  def putAll(x: java.util.Map[K, V]): Unit = {
    // Call our own method so as to insure no nulls find their way in.
    x.foreach { case (k, v) => put(k, v) }
  }
  def remove(x: Any): Maybe[V] = Maybe(javaMap.remove(x))
  def size(): Int = javaMap.size()
  def values(): Iterable[V] = javaMap.values()

}
