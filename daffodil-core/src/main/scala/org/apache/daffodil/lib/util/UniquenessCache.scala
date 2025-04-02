/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.apache.daffodil.lib.util

/**
 * abstract class UniquenessCache was copied from this Scala source file:
 *   https://github.com/scala/scala/blob/904e3a5d2b9616b9c533d77d0c51652b138e8659/src/library/scala/Symbol.scala
 * and it should be kept up to date with the Scala 2.12.x branch:
 *   https://raw.githubusercontent.com/scala/scala/2.12.x/src/library/scala/Symbol.scala
 *
 * This class provides a simple way to get unique objects for equal strings.
 * Because they are interned, they can be compared using reference equality.
 *
 *  @author  Martin Odersky, Iulian Dragos
 *  @since   1.7
 */
abstract class UniquenessCache[K, V >: Null] {
  import java.lang.ref.WeakReference
  import java.util.WeakHashMap
  import java.util.concurrent.locks.ReentrantReadWriteLock

  private val rwl = new ReentrantReadWriteLock()
  private val rlock = rwl.readLock
  private val wlock = rwl.writeLock
  private val map = new WeakHashMap[K, WeakReference[V]]

  protected def valueFromKey(k: K): V
  protected def keyFromValue(v: V): Option[K]

  def apply(name: K): V = {
    def cached(): V = {
      rlock.lock
      try {
        val reference = map.get(name)
        if (reference == null) null
        else reference.get // will be null if we were gc-ed
      } finally rlock.unlock
    }
    def updateCache(): V = {
      wlock.lock
      try {
        val res = cached()
        if (res != null) res
        else {
          // If we don't remove the old String key from the map, we can
          // wind up with one String as the key and a different String as
          // the name field in the Symbol, which can lead to surprising GC
          // behavior and duplicate Symbols. See scala/bug#6706.
          map.remove(name)
          val sym = valueFromKey(name)
          map.put(name, new WeakReference(sym))
          sym
        }
      } finally wlock.unlock
    }

    val res = cached()
    if (res == null) updateCache()
    else res
  }
  def unapply(other: V): Option[K] = keyFromValue(other)
}
