package edu.illinois.ncsa.daffodil.util

import edu.illinois.ncsa.daffodil.exceptions.Assert
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
